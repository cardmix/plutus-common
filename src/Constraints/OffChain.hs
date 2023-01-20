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

import           Control.Monad                    (Monad, liftM2, when)
import           Control.Monad.State              (MonadState (..))
import           Data.Functor                     (($>))
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromJust)
import           Data.Text                        (Text)
import           Ledger                           (Versioned, mintingPolicyHash, validatorHash, interval, DecoratedTxOut)
import           Ledger.Address                   (PaymentPubKeyHash)
import           Ledger.Constraints.TxConstraints
import           Ledger.Constraints.OffChain      (unspentOutputs, plutusV2MintingPolicy, plutusV2OtherScript, otherData, mintingPolicy, otherScript)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, toList, fromInteger, mconcat, mempty)
import           Prelude                          (Semigroup, (<>), mempty)

import           Constraints.CoinSelection        (CoinSelectionBudget, CoinSelectionParams, genCoinSelection)
import           Types.Tx                         (TxConstructor (..), TxConstructorError (..), TransactionBuilder)
import Utils.ChainIndex (MapUTXO)

(<&&>) :: (Semigroup a, Monad m) => m a -> m a -> m a
(<&&>) = liftM2 (<>)

-- If Nothing is passed as the 3rd argument, adds a specific error to the list and sets txConstructorResult to Nothing.
failTx :: Text -> Text -> Maybe res -> TransactionBuilder (Maybe res)
failTx eIn eReason r = if isJust r
    then return r
    else do
        constr <- get
        let errorList = txConstructorErrors constr
        put constr { txConstructorErrors = TxConstructorError eIn eReason : errorList, txConstructorResult = Nothing }
        return r

utxosSpentPublicKeyTx :: [TxOutRef] -> TransactionBuilder ()
utxosSpentPublicKeyTx refs = do
    mapM_ (\ref -> utxoSpentPublicKeyTx (\r _ -> ref == r)) refs
    constr <- get
    when (isNothing $ txConstructorResult constr) $
        failTx "utxosSpentPublicKeyTx" "Cannot spend all provided references" Nothing $> ()

utxoSpentPublicKeyTx :: (TxOutRef -> DecoratedTxOut -> Bool) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentPublicKeyTx f = utxoSpentPublicKeyTx' f >>= failTx "utxoSpentPublicKeyTx" "No matching utxos found"

utxoSpentPublicKeyTx' :: (TxOutRef -> DecoratedTxOut -> Bool) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentPublicKeyTx' f = do
    constr <- get
    let utxos   = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Map.filterWithKey f utxos
    if Map.null utxos'
        then return Nothing
        else do
            let utxo = head $ Map.toList utxos'
                ref  = fst utxo
            put constr { txConstructorResult = res <&&> Just (unspentOutputs (Map.fromList [utxo]), mustSpendPubKeyOutput ref),
                txConstructorLookups = Map.delete ref utxos }
            return $ Just utxo

utxoSpentScriptTx :: ToData redeemer => (TxOutRef -> DecoratedTxOut -> Bool) -> (TxOutRef -> DecoratedTxOut -> Validator) ->
    (TxOutRef -> DecoratedTxOut -> redeemer) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentScriptTx f scriptVal red = utxoSpentScriptTx' f scriptVal red >>= failTx "utxoSpentScriptTx" "No matching utxos found"

utxoSpentScriptTx' :: ToData redeemer => (TxOutRef -> DecoratedTxOut -> Bool) -> (TxOutRef -> DecoratedTxOut -> Validator) ->
    (TxOutRef -> DecoratedTxOut -> redeemer) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentScriptTx' f scriptVal red = do
    constr <- get
    let utxos   = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Map.filterWithKey f utxos
    if Map.null utxos'
        then return Nothing
        else do
            let utxo = head $ Map.toList utxos'
                ref  = fst utxo
            put constr { txConstructorResult = res <&&> Just (unspentOutputs (Map.fromList [utxo]) <> plutusV2OtherScript (uncurry scriptVal utxo),
                        mustSpendScriptOutput ref (Redeemer $ toBuiltinData $ uncurry red utxo)),
                txConstructorLookups = Map.delete ref utxos }
            return $ Just utxo

utxoReferencedTx :: (TxOutRef -> DecoratedTxOut -> Bool) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoReferencedTx f = utxoReferencedTx' f >>= failTx "utxoReferencedTx" "No matching utxos found"

utxoReferencedTx' :: (TxOutRef -> DecoratedTxOut -> Bool) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoReferencedTx' f = do
    constr <- get
    let utxos   = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Map.filterWithKey f utxos
    if Map.null utxos'
        then return Nothing
        else do
            let utxo = head $ Map.toList utxos'
                ref  = fst utxo
            put constr { txConstructorResult = res <&&> Just (unspentOutputs (Map.fromList [utxo]), mustReferenceOutput ref),
                txConstructorLookups = Map.delete ref utxos }
            return $ Just utxo

utxoProducedPublicKeyTx :: ToData datum => PaymentPubKeyHash -> Maybe StakingCredential -> Value -> Maybe datum -> TransactionBuilder ()
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

utxoProducedScriptTx :: ToData datum => ValidatorHash -> Maybe StakingCredential -> Value -> datum -> TransactionBuilder ()
utxoProducedScriptTx vh skc val dat = do
    constr <- get
    let res = txConstructorResult constr
        c | isJust skc = mustPayToOtherScriptAddressWithDatumHash vh (fromJust skc) (Datum $ toBuiltinData dat) val
          | otherwise  = mustPayToOtherScriptWithDatumHash vh (Datum $ toBuiltinData dat) val
    put constr { txConstructorResult = res <&&> Just (mempty, c) }

tokensMintedTx :: ToData redeemer => MintingPolicy -> redeemer -> Value -> TransactionBuilder ()
tokensMintedTx mp red v = do
    constr <- get
    let res = txConstructorResult constr
    put constr { txConstructorResult = res <&&> Just (plutusV2MintingPolicy mp, mustMintValueWithRedeemer (Redeemer $ toBuiltinData red) v) }

validatedInIntervalTx :: POSIXTime -> POSIXTime -> TransactionBuilder ()
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

postValidatorTx :: ToData datum => Address -> Versioned Validator -> Maybe datum -> Value -> TransactionBuilder ()
postValidatorTx addr vld dat val = do
    constr <- get
    let res     = txConstructorResult constr
        hash    = validatorHash vld
        c       = mustPayToAddressWithReferenceValidator addr hash (fmap (TxOutDatumHash . Datum . toBuiltinData) dat) val
        lookups = otherScript vld
    put constr { txConstructorResult = res <&&> Just (lookups, c)}

postMintingPolicyTx :: ToData datum => Address -> Versioned MintingPolicy -> Maybe datum -> Value -> TransactionBuilder ()
postMintingPolicyTx addr mp dat val = do
    constr <- get
    let res     = txConstructorResult constr
        hash    = mintingPolicyHash mp
        c       = mustPayToAddressWithReferenceMintingPolicy addr hash (fmap (TxOutDatumHash . Datum . toBuiltinData) dat) val
        lookups = mintingPolicy mp
    put constr { txConstructorResult = res <&&> Just (lookups, c)}

referenceValidatorTx :: Validator -> TxOutRef -> TransactionBuilder ()
referenceValidatorTx val txOutRef = do
    constr <- get
    let res     = txConstructorResult constr
        lookups = plutusV2OtherScript val
    put constr { txConstructorResult = res <&&> Just (lookups, mustReferenceOutput txOutRef) }

referenceMintingPolicyTx :: ToData redeemer => MintingPolicy -> TxOutRef -> redeemer -> Value -> TransactionBuilder ()
referenceMintingPolicyTx mp txOutRef red v = do
    constr <- get
    let res     = txConstructorResult constr
        lookups = plutusV2MintingPolicy mp
        c       = mustMintValueWithRedeemer (Redeemer $ toBuiltinData red) v <> mustReferenceOutput txOutRef
    put constr { txConstructorResult = res <&&> Just (lookups, c) }

datumTx :: ToData a => a -> TransactionBuilder ()
datumTx a = do
    constr <- get
    let res = txConstructorResult constr
        dat = Datum $ toBuiltinData a
    put constr { txConstructorResult = res <&&> Just (otherData dat, mempty) }

mustBeSignedByTx :: PaymentPubKeyHash -> TransactionBuilder ()
mustBeSignedByTx pkh = do
    constr <- get
    let res = txConstructorResult constr
    put constr { txConstructorResult = res <&&> Just (mempty, mustBeSignedBy pkh) }

-- Ensures that transaction creator spends enough to auto-balance transaction
prebalanceTx :: CoinSelectionBudget -> CoinSelectionParams -> MapUTXO -> TransactionBuilder ()
prebalanceTx budget params walletUTXO = case genCoinSelection budget params walletUTXO of
      Nothing  -> failTx "balanceTx" "Cannot make a coin selection." Nothing $> ()
      Just sel -> utxosSpentPublicKeyTx (Map.keys sel)