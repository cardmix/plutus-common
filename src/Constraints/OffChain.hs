{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Constraints.OffChain where

import           Control.Monad                    (Monad, liftM2)
import           Control.Monad.State              (State, MonadState (..))
import           Data.Maybe                       (fromJust)
import qualified Data.Map
import           Data.Text                        (Text)
import           Ledger                           (Versioned, mintingPolicyHash, validatorHash, interval, DecoratedTxOut)
import           Ledger.Address                   (PaymentPubKeyHash)
import           Ledger.Constraints.TxConstraints
import           Ledger.Constraints.OffChain      (unspentOutputs, plutusV2MintingPolicy, plutusV2OtherScript, otherData, mintingPolicy, otherScript)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, toList, fromInteger, mconcat, mempty)
import           Prelude                          (Semigroup, (<>), mempty)
import           Types.Tx                         (TxConstructor (..), TxConstructorError (..))

(<&&>) :: (Semigroup a, Monad m) => m a -> m a -> m a
(<&&>) = liftM2 (<>)

-- If Nothing is passed as the 3rd argument, adds a specific error to the list and sets txConstructorResult to Nothing.
failTx :: Text -> Text -> Maybe res -> State (TxConstructor a i o) (Maybe res)
failTx eIn eReason r = if isJust r
    then return r
    else do
        constr <- get
        let errorList = txConstructorErrors constr
        put constr { txConstructorErrors = TxConstructorError eIn eReason : errorList, txConstructorResult = Nothing }
        return r

utxoSpentPublicKeyTx :: (TxOutRef -> DecoratedTxOut -> Bool) -> State (TxConstructor a i o) (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentPublicKeyTx f = utxoSpentPublicKeyTx' f >>= failTx "utxoSpentPublicKeyTx" "No matching utxos found"

utxoSpentPublicKeyTx' :: (TxOutRef -> DecoratedTxOut -> Bool) -> State (TxConstructor a i o) (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentPublicKeyTx' f = do
    constr <- get
    let utxos   = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Data.Map.filterWithKey f utxos
    if Data.Map.null utxos'
        then return Nothing
        else do
            let utxo = head $ Data.Map.toList utxos'
                ref  = fst utxo
            put constr { txConstructorResult = res <&&> Just (unspentOutputs (Data.Map.fromList [utxo]), mustSpendPubKeyOutput ref),
                txConstructorLookups = Data.Map.delete ref utxos }
            return $ Just utxo

utxoSpentScriptTx :: ToData redeemer => (TxOutRef -> DecoratedTxOut -> Bool) -> (TxOutRef -> DecoratedTxOut -> Validator) ->
    (TxOutRef -> DecoratedTxOut -> redeemer) -> State (TxConstructor a i o) (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentScriptTx f scriptVal red = utxoSpentScriptTx' f scriptVal red >>= failTx "utxoSpentScriptTx" "No matching utxos found"

utxoSpentScriptTx' :: ToData redeemer => (TxOutRef -> DecoratedTxOut -> Bool) -> (TxOutRef -> DecoratedTxOut -> Validator) ->
    (TxOutRef -> DecoratedTxOut -> redeemer) -> State (TxConstructor a i o) (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentScriptTx' f scriptVal red = do
    constr <- get
    let utxos   = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Data.Map.filterWithKey f utxos
    if Data.Map.null utxos'
        then return Nothing
        else do
            let utxo = head $ Data.Map.toList utxos'
                ref  = fst utxo
            put constr { txConstructorResult = res <&&> Just (unspentOutputs (Data.Map.fromList [utxo]) <> plutusV2OtherScript (uncurry scriptVal utxo),
                        mustSpendScriptOutput ref (Redeemer $ toBuiltinData $ uncurry red utxo)),
                txConstructorLookups = Data.Map.delete ref utxos }
            return $ Just utxo

utxoReferencedTx :: (TxOutRef -> DecoratedTxOut -> Bool) -> State (TxConstructor a i o) (Maybe (TxOutRef, DecoratedTxOut))
utxoReferencedTx f = utxoReferencedTx' f >>= failTx "utxoReferencedTx" "No matching utxos found"

utxoReferencedTx' :: (TxOutRef -> DecoratedTxOut -> Bool) -> State (TxConstructor a i o) (Maybe (TxOutRef, DecoratedTxOut))
utxoReferencedTx' f = do
    constr <- get
    let utxos   = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Data.Map.filterWithKey f utxos
    if Data.Map.null utxos'
        then return Nothing
        else do
            let utxo = head $ Data.Map.toList utxos'
                ref  = fst utxo
            put constr { txConstructorResult = res <&&> Just (unspentOutputs (Data.Map.fromList [utxo]), mustReferenceOutput ref),
                txConstructorLookups = Data.Map.delete ref utxos }
            return $ Just utxo

utxoProducedPublicKeyTx :: ToData datum => PaymentPubKeyHash -> Maybe StakingCredential -> Value -> Maybe datum -> State (TxConstructor a i o) ()
utxoProducedPublicKeyTx pkh skc val dat = do
    constr <- get
    let res = txConstructorResult constr
        c | isJust skc = if isJust dat
            then mustPayToPubKeyAddressWithDatumHash pkh (fromJust skc) (Datum $ toBuiltinData $ fromJust dat) val
            else mustPayToPubKeyAddress pkh (fromJust skc) val
          | otherwise = if isJust dat
            then mustPayToPubKeyWithDatumHash pkh (Datum $ toBuiltinData $ fromJust dat) val
            else mustPayToPubKey pkh val
    put constr { txConstructorResult = res <&&> Just (mempty, c) }

utxoProducedScriptTx :: ToData datum => ValidatorHash -> Maybe StakingCredential -> Value -> datum -> State (TxConstructor a i o) ()
utxoProducedScriptTx vh skc val dat = do
    constr <- get
    let res = txConstructorResult constr
        c | isJust skc = mustPayToOtherScriptAddressWithDatumHash vh (fromJust skc) (Datum $ toBuiltinData dat) val
          | otherwise  = mustPayToOtherScriptWithDatumHash vh (Datum $ toBuiltinData dat) val
    put constr { txConstructorResult = res <&&> Just (mempty, c) }

tokensMintedTx :: ToData redeemer => MintingPolicy -> redeemer -> Value -> State (TxConstructor a i o) ()
tokensMintedTx mp red v = do
    constr <- get
    let res = txConstructorResult constr
    put constr { txConstructorResult = res <&&> Just (plutusV2MintingPolicy mp, mustMintValueWithRedeemer (Redeemer $ toBuiltinData red) v) }

validatedInIntervalTx :: POSIXTime -> POSIXTime -> State (TxConstructor a i o) ()
validatedInIntervalTx startTime endTime = do
    constr <- get
    let ct   = txCurrentTime constr
        res  = txConstructorResult constr
        cond = startTime <= ct &&  ct <= endTime
    if cond
        then put constr { txConstructorResult = res <&&> Just (mempty, mustValidateIn $ interval startTime endTime) }
        else do
            _ <- failTx "validatedInIntervalTx" "Current time is not in the interval" Nothing
            return ()

postValidatorTx :: ToData datum => Address -> Versioned Validator -> Maybe datum -> Value -> State (TxConstructor a i o) ()
postValidatorTx addr vld dat val = do
    constr <- get
    let res     = txConstructorResult constr
        hash    = validatorHash vld
        c       = mustPayToAddressWithReferenceValidator addr hash (fmap (TxOutDatumHash . Datum . toBuiltinData) dat) val
        lookups = otherScript vld
    put constr { txConstructorResult = res <&&> Just (lookups, c)}

postMintingPolicyTx :: ToData datum => Address -> Versioned MintingPolicy -> Maybe datum -> Value -> State (TxConstructor a i o) ()
postMintingPolicyTx addr mp dat val = do
    constr <- get
    let res     = txConstructorResult constr
        hash    = mintingPolicyHash mp
        c       = mustPayToAddressWithReferenceMintingPolicy addr hash (fmap (TxOutDatumHash . Datum . toBuiltinData) dat) val
        lookups = mintingPolicy mp
    put constr { txConstructorResult = res <&&> Just (lookups, c)}

referenceValidatorTx :: Validator -> TxOutRef -> State (TxConstructor a i o) ()
referenceValidatorTx val txOutRef = do
    constr <- get
    let res     = txConstructorResult constr
        lookups = plutusV2OtherScript val
    put constr { txConstructorResult = res <&&> Just (lookups, mustReferenceOutput txOutRef) }

referenceMintingPolicyTx :: ToData redeemer => MintingPolicy -> TxOutRef -> redeemer -> Value -> State (TxConstructor a i o) ()
referenceMintingPolicyTx mp txOutRef red v = do
    constr <- get
    let res     = txConstructorResult constr
        lookups = plutusV2MintingPolicy mp
        c       = mustMintValueWithRedeemer (Redeemer $ toBuiltinData red) v <> mustReferenceOutput txOutRef
    put constr { txConstructorResult = res <&&> Just (lookups, c) }

datumTx :: ToData a => a -> State (TxConstructor a i o) ()
datumTx a = do
    constr <- get
    let res = txConstructorResult constr
        dat = Datum $ toBuiltinData a
    put constr { txConstructorResult = res <&&> Just (otherData dat, mempty) }

mustBeSignedByTx :: PaymentPubKeyHash -> State (TxConstructor a i o) ()
mustBeSignedByTx pkh = do
    constr <- get
    let res = txConstructorResult constr
    put constr { txConstructorResult = res <&&> Just (mempty, mustBeSignedBy pkh) }