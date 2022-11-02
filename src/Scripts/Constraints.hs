{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Scripts.Constraints where

import           Control.Monad.State              (State, MonadState (..))
import           Data.Maybe                       (fromJust)
import qualified Data.Map
import           Ledger                           (ChainIndexTxOut, Versioned, mintingPolicyHash, validatorHash, contains, interval)
import           Ledger.Address                   (PaymentPubKeyHash, StakePubKeyHash)
import           Ledger.Constraints.TxConstraints 
import           Ledger.Constraints.OffChain      (unspentOutputs, plutusV2MintingPolicy, plutusV2OtherScript, otherData, mintingPolicy, otherScript)
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts        (findDatum, findOwnInput, ownCurrencySymbol)
import           PlutusTx.AssocMap                (lookup)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, toList, fromInteger, mempty)
import           Prelude                          ((<>), mempty)

import           Types.TxConstructor              (TxConstructor (..))

----------------------------- On-Chain -------------------------------

{-# INLINABLE checkDatum #-}
checkDatum :: FromData a => TxInfo -> (a -> Bool) -> Maybe TxOut -> Bool
checkDatum info f x = fromMaybe False $ do
    o   <- x
    dat <- case txOutDatum o of
      OutputDatum d      -> Just d
      OutputDatumHash dh -> findDatum dh info
      NoOutputDatum      -> Nothing
    fmap f $ fromBuiltinData $ getDatum dat

{-# INLINABLE findUtxoSpent #-}
findUtxoSpent :: TxInfo -> (TxOut -> Bool) -> Maybe TxOut
findUtxoSpent info f = find f ins
    where ins = map txInInfoResolved $ txInfoInputs info

{-# INLINABLE filterUtxoSpent #-}
filterUtxoSpent :: TxInfo -> (TxOut -> Bool) -> [TxOut]
filterUtxoSpent info f = filter f ins
    where ins = map txInInfoResolved $ txInfoInputs info

{-# INLINABLE utxoSpent #-}
utxoSpent :: TxInfo -> (TxOut -> Bool) -> Bool
utxoSpent info = isJust . findUtxoSpent info

{-# INLINABLE findUtxoReferenced #-}
findUtxoReferenced :: TxInfo -> (TxOut -> Bool) -> Maybe TxOut
findUtxoReferenced info f = find f ins
    where ins = map txInInfoResolved $ txInfoReferenceInputs info

{-# INLINABLE filterUtxoReferenced #-}
filterUtxoReferenced :: TxInfo -> (TxOut -> Bool) -> [TxOut]
filterUtxoReferenced info f = filter f ins
    where ins = map txInInfoResolved $ txInfoReferenceInputs info

{-# INLINABLE utxoReferenced #-}
utxoReferenced :: TxInfo -> (TxOut -> Bool) -> Bool
utxoReferenced info = isJust . findUtxoReferenced info

{-# INLINABLE findUtxoProduced #-}
findUtxoProduced :: TxInfo -> (TxOut -> Bool) -> Maybe TxOut
findUtxoProduced info f = find f outs
    where outs = txInfoOutputs info

{-# INLINABLE filterUtxoProduced #-}
filterUtxoProduced :: TxInfo -> (TxOut -> Bool) -> [TxOut]
filterUtxoProduced info f = filter f outs
    where outs = txInfoOutputs info

{-# INLINABLE utxoProduced #-}
utxoProduced :: TxInfo -> (TxOut -> Bool) -> Bool
utxoProduced info = isJust . findUtxoProduced info

{-# INLINABLE utxoProducedNumberEq #-}
utxoProducedNumberEq :: TxInfo -> (TxOut -> Bool) -> Integer -> Bool
utxoProducedNumberEq info f n = length (filter f outs) == n
    where outs = txInfoOutputs info

{-# INLINABLE checkOwnInput #-}
checkOwnInput :: ScriptContext -> (TxOut -> Bool) -> Bool
checkOwnInput ctx f = fromMaybe False $ do
    o <- findOwnInput ctx
    return $ f $ txInInfoResolved o

{-# INLINABLE currencyMintedOrBurned #-}
currencyMintedOrBurned :: TxInfo -> CurrencySymbol -> Bool
currencyMintedOrBurned info cs = maybe False (not . null) $ lookup cs $ getValue $ txInfoMint info

-- PlutusTx AssocMap is not sorted automatically.
-- Therefore, the second argument of tokensMinted must be pre-sorted manually (preferably off-chain).
{-# INLINABLE tokensMinted #-}
tokensMinted :: ScriptContext -> Map TokenName Integer -> Bool
tokensMinted ctx expected = actual == Just expected
    where
        ownCS  = ownCurrencySymbol ctx
        actual = lookup ownCS $ getValue $ txInfoMint $ scriptContextTxInfo ctx

{-# INLINABLE validatedInInterval #-}
validatedInInterval :: TxInfo -> POSIXTime -> POSIXTime ->  Bool
validatedInInterval info startTime endTime = intDeclared `contains` intActual
    where
        intActual   = txInfoValidRange info
        intDeclared = interval startTime endTime

{-# INLINABLE getUpperTimeEstimate #-}
getUpperTimeEstimate :: TxInfo -> POSIXTime
getUpperTimeEstimate info = case ivTo (txInfoValidRange info) of
                                UpperBound (Finite t) _ -> t
                                _                       -> error ()

{-# INLINABLE getLowerTimeEstimate #-}
getLowerTimeEstimate :: TxInfo -> POSIXTime
getLowerTimeEstimate info = case ivFrom (txInfoValidRange info) of
                                LowerBound (Finite t) _ -> t
                                _                       -> error ()

-------------------------- Off-Chain -----------------------------

failTx :: Maybe res -> State (TxConstructor a i o) (Maybe res)
failTx r = if isJust r
    then return r
    else do
        constr <- get
        put constr { txConstructorResult = Nothing }
        return r

utxoSpentPublicKeyTx :: (TxOutRef -> ChainIndexTxOut -> Bool) -> State (TxConstructor a i o) (Maybe (TxOutRef, ChainIndexTxOut))
utxoSpentPublicKeyTx f = utxoSpentPublicKeyTx' f >>= failTx

utxoSpentPublicKeyTx' :: (TxOutRef -> ChainIndexTxOut -> Bool) -> State (TxConstructor a i o) (Maybe (TxOutRef, ChainIndexTxOut))
utxoSpentPublicKeyTx' f = do
    constr <- get
    let lookups = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos   = Data.Map.filterWithKey f $ Data.Map.map fst lookups
    if Data.Map.null utxos
        then return Nothing
        else do
            let utxo = head $ Data.Map.toList utxos
                ref  = fst utxo
            put constr { txConstructorResult = res <> Just (unspentOutputs (Data.Map.fromList [utxo]), mustSpendPubKeyOutput ref),
                txConstructorLookups = Data.Map.delete ref lookups }
            return $ Just utxo

utxoSpentScriptTx :: ToData redeemer => (TxOutRef -> ChainIndexTxOut -> Bool) -> (TxOutRef -> ChainIndexTxOut -> Validator) ->
    (TxOutRef -> ChainIndexTxOut -> redeemer) -> State (TxConstructor a i o) (Maybe (TxOutRef, ChainIndexTxOut))
utxoSpentScriptTx f scriptVal red = utxoSpentScriptTx' f scriptVal red >>= failTx

utxoSpentScriptTx' :: ToData redeemer => (TxOutRef -> ChainIndexTxOut -> Bool) -> (TxOutRef -> ChainIndexTxOut -> Validator) ->
    (TxOutRef -> ChainIndexTxOut -> redeemer) -> State (TxConstructor a i o) (Maybe (TxOutRef, ChainIndexTxOut))
utxoSpentScriptTx' f scriptVal red = do
    constr <- get
    let lookups = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos   = Data.Map.filterWithKey f $ Data.Map.map fst lookups
    if Data.Map.null utxos
        then return Nothing
        else do
            let utxo = head $ Data.Map.toList utxos
                ref  = fst utxo
            put constr { txConstructorResult = res <> Just (unspentOutputs (Data.Map.fromList [utxo]) <> plutusV2OtherScript (uncurry scriptVal utxo),
                mustSpendScriptOutput ref (Redeemer $ toBuiltinData $ uncurry red utxo)),
                txConstructorLookups = Data.Map.delete ref lookups }
            return $ Just utxo

utxoReferencedTx :: (TxOutRef -> ChainIndexTxOut -> Bool) -> State (TxConstructor a i o) (Maybe (TxOutRef, ChainIndexTxOut))
utxoReferencedTx f = utxoReferencedTx' f >>= failTx

utxoReferencedTx' :: (TxOutRef -> ChainIndexTxOut -> Bool) -> State (TxConstructor a i o) (Maybe (TxOutRef, ChainIndexTxOut))
utxoReferencedTx' f = do
    constr <- get
    let lookups = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos   = Data.Map.filterWithKey f $ Data.Map.map fst lookups
    if Data.Map.null utxos
        then return Nothing
        else do
            let utxo = head $ Data.Map.toList utxos
                ref  = fst utxo
            put constr { txConstructorResult = res <> Just (unspentOutputs (Data.Map.fromList [utxo]), mustReferenceOutput ref),
                txConstructorLookups = Data.Map.delete ref lookups }
            return $ Just utxo

utxoProducedPublicKeyTx :: ToData datum => PaymentPubKeyHash -> Maybe StakePubKeyHash -> Value -> Maybe datum -> State (TxConstructor a i o) ()
utxoProducedPublicKeyTx pkh skh val dat = do
    constr <- get
    let res = txConstructorResult constr
        c | isJust skh = if isJust dat
            then mustPayWithDatumToPubKeyAddress pkh (fromJust skh) (Datum $ toBuiltinData $ fromJust dat) val
            else mustPayToPubKeyAddress pkh (fromJust skh) val
          | otherwise = if isJust dat
            then mustPayWithDatumToPubKey pkh (Datum $ toBuiltinData $ fromJust dat) val
            else mustPayToPubKey pkh val
    put constr { txConstructorResult = res <> Just (mempty, c) }

utxoProducedScriptTx :: ToData datum => ValidatorHash -> Maybe StakeValidatorHash -> Value -> datum -> State (TxConstructor a i o) ()
utxoProducedScriptTx vh svh val dat = do
    constr <- get
    let res = txConstructorResult constr
        c | isJust svh = mustPayToOtherScriptAddress vh (fromJust svh) (Datum $ toBuiltinData dat) val
          | otherwise  = mustPayToOtherScript vh (Datum $ toBuiltinData dat) val
    put constr { txConstructorResult = res <> Just (mempty, c) }

tokensMintedTx :: ToData redeemer => MintingPolicy -> redeemer -> Value -> State (TxConstructor a i o) ()
tokensMintedTx mp red v = do
    constr <- get
    let res = txConstructorResult constr
    put constr { txConstructorResult = res <> Just (plutusV2MintingPolicy mp, mustMintValueWithRedeemer (Redeemer $ toBuiltinData red) v) }

validatedInIntervalTx :: POSIXTime -> POSIXTime -> State (TxConstructor a i o) ()
validatedInIntervalTx startTime endTime = do
    constr <- get
    let ct   = txCurrentTime constr
        res  = txConstructorResult constr
        cond = startTime <= ct && ct <= endTime
    if cond
        then put constr { txConstructorResult = res <> Just (mempty, mustValidateIn $ interval startTime endTime) }
        else put constr { txConstructorResult = Nothing }

postValidatorTx :: ToData datum => Address -> Versioned Validator -> Maybe datum -> Value -> State (TxConstructor a i o) ()
postValidatorTx addr vld dat val = do
    constr <- get
    let res     = txConstructorResult constr
        hash    = validatorHash vld
        c       = mustPayToAddressWithReferenceValidator addr hash (fmap (TxOutDatumHash . Datum . toBuiltinData) dat) val
        lookups = otherScript vld
    put constr { txConstructorResult = res <> Just (lookups, c)}

postMintingPolicyTx :: ToData datum => Address -> Versioned MintingPolicy -> Maybe datum -> Value -> State (TxConstructor a i o) ()
postMintingPolicyTx addr mp dat val = do
    constr <- get
    let res     = txConstructorResult constr
        hash    = mintingPolicyHash mp
        c       = mustPayToAddressWithReferenceMintingPolicy addr hash (fmap (TxOutDatumHash . Datum . toBuiltinData) dat) val
        lookups = mintingPolicy mp
    put constr { txConstructorResult = res <> Just (lookups, c)}

referenceValidatorTx :: Validator -> TxOutRef -> State (TxConstructor a i o) ()
referenceValidatorTx val txOutRef = do
    constr <- get
    let res     = txConstructorResult constr
        lookups = plutusV2OtherScript val
    put constr { txConstructorResult = res <> Just (lookups, mustReferenceOutput txOutRef) }

referenceMintingPolicyTx :: ToData redeemer => MintingPolicy -> TxOutRef -> redeemer -> Value -> State (TxConstructor a i o) ()
referenceMintingPolicyTx mp txOutRef red v = do
    constr <- get
    let res     = txConstructorResult constr
        lookups = plutusV2MintingPolicy mp
        c       = mustMintValueWithRedeemer (Redeemer $ toBuiltinData red) v <> mustReferenceOutput txOutRef
    put constr { txConstructorResult = res <> Just (lookups, c) }

datumTx :: ToData a => a -> State (TxConstructor a i o) ()
datumTx a = do
    constr <- get
    let res = txConstructorResult constr
        dat = Datum $ toBuiltinData a
    put constr { txConstructorResult = res <> Just (otherData dat, mempty) }

mustBeSignedByTx :: PaymentPubKeyHash -> State (TxConstructor a i o) ()
mustBeSignedByTx pkh = do
    constr <- get
    let res = txConstructorResult constr
    put constr { txConstructorResult = res <> Just (mempty, mustBeSignedBy pkh) }