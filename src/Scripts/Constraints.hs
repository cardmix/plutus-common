{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Scripts.Constraints where

import           Control.Monad.State              (State, state)
import           Data.Maybe                       (fromJust)
import qualified Data.Map
import           Ledger                           hiding (singleton, unspentOutputs, lookup)
import           Ledger.Constraints.TxConstraints (mustSpendPubKeyOutput, mustSpendScriptOutput, mustPayWithDatumToPubKey, mustPayWithDatumToPubKeyAddress,
                                                    mustPayToOtherScriptAddress, mustPayToOtherScript, mustValidateIn, mustMintValueWithRedeemer)
import           Ledger.Constraints.OffChain      (unspentOutputs, mintingPolicy, otherScript)
import           Ledger.Value                     (getValue)
import           Plutus.V1.Ledger.Api             (FromData(..))
import           PlutusTx.AssocMap                (Map, lookup, toList)
import           PlutusTx.IsData                  (ToData(..))
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, toList, fromInteger, mempty)
import           Prelude                          ((<>), mempty, undefined)

import           Types.TxConstructor

----------------------------- On-Chain -------------------------------

{-# INLINABLE checkDatum #-}
checkDatum :: FromData a => TxInfo -> (a -> Bool) -> Maybe TxOut -> Bool
checkDatum info f x = fromMaybe False $ do
    o   <- x
    dh  <- txOutDatumHash o
    dat <- findDatum dh info
    fmap f $ fromBuiltinData $ getDatum dat

{-# INLINABLE findUtxoSpent #-}
findUtxoSpent :: TxInfo -> (TxOut -> Bool) -> Maybe TxOut
findUtxoSpent info f = find f ins
    where ins = map txInInfoResolved $ txInfoInputs info

{-# INLINABLE utxoSpent #-}
utxoSpent :: TxInfo -> (TxOut -> Bool) -> Bool
utxoSpent info = isJust . findUtxoSpent info

-- TODO: implement this
{-# INLINABLE findUtxoReferenced #-}
findUtxoReferenced :: TxInfo -> (TxOut -> Bool) -> Maybe TxOut
findUtxoReferenced _ _ = Nothing

{-# INLINABLE utxoReferenced #-}
utxoReferenced :: TxInfo -> (TxOut -> Bool) -> Bool
utxoReferenced info = isJust . findUtxoReferenced info

{-# INLINABLE findUtxoProduced #-}
findUtxoProduced :: TxInfo -> (TxOut -> Bool) -> Maybe TxOut
findUtxoProduced info f = find f outs
    where outs = txInfoOutputs info

{-# INLINABLE utxoProduced #-}
utxoProduced :: TxInfo -> (TxOut -> Bool) -> Bool
utxoProduced info = isJust . findUtxoProduced info

{-# INLINABLE utxoProducedNumberEq #-}
utxoProducedNumberEq :: TxInfo -> (TxOut -> Bool) -> Integer -> Bool
utxoProducedNumberEq info f n = length (filter f outs) == n
    where outs = txInfoOutputs info

{-# INLINABLE utxoProducedInjectiveTxOutRef #-}
utxoProducedInjectiveTxOutRef :: forall a . (FromData a) => ScriptContext -> (TxOut -> Bool) -> (a -> TxOutRef) -> Bool
utxoProducedInjectiveTxOutRef ctx f g  = isJust $ find (\o -> f o && txOutDatumHash o == dh && isJust dh) outs
    where
        info = scriptContextTxInfo ctx
        outs = txInfoOutputs info
        ownRef = case scriptContextPurpose ctx of
          Spending ref -> ref
          _            -> error ()
        dh = fmap fst $ find (maybe False (== ownRef) . fmap g . fromBuiltinData . getDatum . snd) (txInfoData info)

{-# INLINABLE utxoProducedInjectiveTokenNames #-}
utxoProducedInjectiveTokenNames :: forall a . (FromData a) => ScriptContext -> (TxOut -> Bool) -> (a -> (TokenName, Integer)) -> Bool
utxoProducedInjectiveTokenNames ctx f g = all (utxoProducedInjectiveTokenName ctx f g) m
    where
        ownCS = ownCurrencySymbol ctx
        m     = toList $ fromMaybe (error ()) $ lookup ownCS $ getValue $ txInfoMint $ scriptContextTxInfo ctx

{-# INLINABLE utxoProducedInjectiveTokenName #-}
utxoProducedInjectiveTokenName :: forall a . (FromData a) => ScriptContext -> (TxOut -> Bool) -> (a -> (TokenName, Integer)) -> (TokenName, Integer) -> Bool
utxoProducedInjectiveTokenName ctx f g p  = isJust $ find (\o -> f o && txOutDatumHash o == dh && isJust dh) outs
    where
        info = scriptContextTxInfo ctx
        outs = txInfoOutputs info
        dh = fmap fst $ find (maybe False (== p) . fmap g . fromBuiltinData . getDatum . snd) (txInfoData info)

{-# INLINABLE checkOwnInput #-}
checkOwnInput :: ScriptContext -> (TxOut -> Bool) -> Bool
checkOwnInput ctx f = fromMaybe False $ do
    o <- findOwnInput ctx
    return $ f $ txInInfoResolved o

{-# INLINABLE currencyMintedOrBurned #-}
currencyMintedOrBurned :: TxInfo -> CurrencySymbol -> Bool
currencyMintedOrBurned info cs = maybe False (not . null) $ lookup cs $ getValue $ txInfoMint info

{-# INLINABLE tokensMinted #-}
tokensMinted :: ScriptContext -> Map TokenName Integer -> Bool
tokensMinted ctx expected = actual == Just expected
    where
        ownCS  = ownCurrencySymbol ctx
        actual = lookup ownCS $ getValue $ txInfoMint $ scriptContextTxInfo ctx

{-# INLINABLE tokensBurned #-}
tokensBurned :: ScriptContext -> Map TokenName Integer -> Bool
tokensBurned ctx expected = actual == Just expected
    where
        ownCS  = ownCurrencySymbol ctx
        actual = lookup ownCS $ getValue $ negate $ txInfoMint $ scriptContextTxInfo ctx

{-# INLINABLE validatedInInterval #-}
validatedInInterval :: TxInfo -> POSIXTime -> POSIXTime ->  Bool
validatedInInterval info startTime endTime = intDeclared `contains` intActual
    where
        intActual   = txInfoValidRange info
        intDeclared = interval startTime endTime

{-# INLINABLE timeToValidate #-}
timeToValidate :: POSIXTime
timeToValidate = 600_000

{-# INLINABLE validatedAround #-}
validatedAround :: TxInfo -> POSIXTime -> Bool
validatedAround info time = validatedInInterval info time (time + timeToValidate)

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

failTx :: Maybe res -> State (TxConstructor a i o) ()
failTx r = if isJust r then return () else state $ \constr -> ((), constr { txConstructorResult = Nothing })

utxoSpentPublicKeyTx :: (TxOutRef -> ChainIndexTxOut -> Bool) -> State (TxConstructor a i o) ()
utxoSpentPublicKeyTx f = utxoSpentPublicKeyTx' f >>= failTx

utxoSpentPublicKeyTx' :: (TxOutRef -> ChainIndexTxOut -> Bool) -> State (TxConstructor a i o) (Maybe (TxOutRef, ChainIndexTxOut))
utxoSpentPublicKeyTx' f = state $ \constr@(TxConstructor _ _ lookups res) -> 
    let utxos  = Data.Map.filterWithKey f $ Data.Map.map fst lookups
    in if Data.Map.null utxos
        then (Nothing, constr)
        else
            let utxo = head $ Data.Map.toList utxos
                ref  = fst utxo
            in (Just utxo, constr { txConstructorResult = res <> Just (unspentOutputs utxos, mustSpendPubKeyOutput ref) })

utxoSpentScriptTx :: ToData r => (TxOutRef -> ChainIndexTxOut -> Bool) -> (TxOutRef -> ChainIndexTxOut -> Validator) ->
    (TxOutRef -> ChainIndexTxOut -> r) -> State (TxConstructor a i o) ()
utxoSpentScriptTx f scriptVal red = utxoSpentScriptTx' f scriptVal red >>= failTx

utxoSpentScriptTx' :: ToData r => (TxOutRef -> ChainIndexTxOut -> Bool) -> (TxOutRef -> ChainIndexTxOut -> Validator) ->
    (TxOutRef -> ChainIndexTxOut -> r) -> State (TxConstructor a i o) (Maybe (TxOutRef, ChainIndexTxOut))
utxoSpentScriptTx' f scriptVal red = state $ \constr@(TxConstructor _ _ lookups res) ->
    let utxos  = Data.Map.filterWithKey f $ Data.Map.map fst lookups
    in if Data.Map.null utxos
        then (Nothing, constr)
        else
            let utxo = head $ Data.Map.toList utxos
                ref  = fst utxo
            in (Just utxo, constr { txConstructorResult = res <> Just (unspentOutputs utxos <> otherScript (uncurry scriptVal utxo),
                mustSpendScriptOutput ref (Redeemer $ toBuiltinData $ uncurry red utxo)) })

utxoReferencedTx :: Bool -> (TxOutRef -> ChainIndexTxOut -> Bool) -> TxConstructor a i o -> TxConstructor a i o
utxoReferencedTx _ _ = undefined

utxoProducedPublicKeyTx :: ToData d => PaymentPubKeyHash -> Maybe StakePubKeyHash -> Value -> d -> State (TxConstructor a i o) ()
utxoProducedPublicKeyTx pkh skh val dat = state $ \constr@(TxConstructor _ _ _ res) ->
    let c = if isJust skh
            then mustPayWithDatumToPubKeyAddress pkh (fromJust skh) (Datum $ toBuiltinData dat) val
            else mustPayWithDatumToPubKey pkh (Datum $ toBuiltinData dat) val
    in ((), constr { txConstructorResult = res <> Just (mempty, c) })

utxoProducedScriptTx :: ToData d => ValidatorHash -> Maybe StakeValidatorHash -> Value -> d -> State (TxConstructor a i o) ()
utxoProducedScriptTx vh svh val dat = state $ \constr@(TxConstructor _ _ _ res) ->
    let c = if isJust svh
            then mustPayToOtherScriptAddress vh (fromJust svh) (Datum $ toBuiltinData dat) val
            else mustPayToOtherScript vh (Datum $ toBuiltinData dat) val
    in ((), constr { txConstructorResult = res <> Just (mempty, c) })

tokensMintedTx :: ToData r => MintingPolicy -> r -> Value -> State (TxConstructor a i o) ()
tokensMintedTx mp red v = state $ \constr@(TxConstructor _ _ _ res) ->
    ((), constr { txConstructorResult = res <> Just (mintingPolicy mp, mustMintValueWithRedeemer (Redeemer $ toBuiltinData red) v) })

tokensBurnedTx :: ToData r => MintingPolicy -> r -> Value -> State (TxConstructor a i o) ()
tokensBurnedTx mp red v = state $ \constr@(TxConstructor _ _ _ res) -> 
    ((), constr { txConstructorResult = res <> Just (mintingPolicy mp, mustMintValueWithRedeemer (Redeemer $ toBuiltinData red) (negate v)) })

validatedInIntervalTx :: POSIXTime -> POSIXTime -> State (TxConstructor a i o) ()
validatedInIntervalTx startTime endTime = state $ \constr@(TxConstructor ct _ _ res) ->
    let cond = startTime <= ct &&  ct <= endTime
    in if cond
        then ((), constr { txConstructorResult = res <> Just (mempty, mustValidateIn $ interval startTime endTime) })
        else ((), constr { txConstructorResult = Nothing })        

validatedAroundTx :: POSIXTime -> State (TxConstructor a i o) ()
validatedAroundTx time = validatedInIntervalTx time (time + timeToValidate)