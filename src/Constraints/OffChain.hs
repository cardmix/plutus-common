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

import           Control.Monad                       (liftM2, when)
import           Control.Monad.State                 (MonadState (..))
import           Data.Functor                        (($>))
import           Data.List                           (find)
import qualified Data.Map                            as Map
import           Data.Maybe                          (isJust, isNothing)
import           Data.Text                           (Text)
import           Ledger                              (DecoratedTxOut(..), Versioned, Slot, mintingPolicyHash, validatorHash)
import           Ledger.Address                      (PaymentPubKeyHash)
import           Ledger.Constraints.OffChain         (unspentOutputs, otherData, mintingPolicy, otherScript)
import           Ledger.Constraints.TxConstraints
import           Ledger.Constraints.ValidityInterval (interval)
import           Plutus.V2.Ledger.Api                hiding (singleton)
import           Prelude                             

import           Types.Error                         (TxBuilderError(..))
import           Types.Tx                            (TxConstructor (..), TransactionBuilder)
import           Utils.ChainIndex                    (filterPubKeyUtxos, filterScriptUtxos)

(<&&>) :: (Semigroup a, Monad m) => m a -> m a -> m a
(<&&>) = liftM2 (<>)

-- If Nothing is passed as the 3rd argument, adds a specific error to the list and sets txConstructorResult to Nothing.
failTx :: Text -> Text -> Maybe res -> TransactionBuilder (Maybe res)
failTx eIn eReason r = if isJust r
    then return r
    else do
        constr <- get
        let errorList = txConstructorErrors constr
        put constr { txConstructorErrors = TxBuilderError eIn eReason : errorList, txConstructorResult = Nothing }
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
    let utxos   = filterPubKeyUtxos $ txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Map.filterWithKey f utxos
    if Map.null utxos'
        then return Nothing
        else do
            let utxo = head $ Map.toList utxos'
                ref  = fst utxo
                lookups = unspentOutputs (Map.fromList [utxo])
                cons    = mustSpendPubKeyOutput ref
            put constr  {
                            txConstructorResult = res <&&> Just (lookups, cons),
                            txConstructorLookups = Map.delete ref utxos
                        }
            return $ Just utxo

utxoSpentScriptTx :: ToData redeemer => (TxOutRef -> DecoratedTxOut -> Bool) -> (TxOutRef -> DecoratedTxOut -> Versioned Validator) ->
    (TxOutRef -> DecoratedTxOut -> redeemer) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentScriptTx f scriptVal red = utxoSpentScriptTx' f scriptVal red >>= failTx "utxoSpentScriptTx" "No matching utxos found"

utxoSpentScriptTx' :: ToData redeemer => (TxOutRef -> DecoratedTxOut -> Bool) -> (TxOutRef -> DecoratedTxOut -> Versioned Validator) ->
    (TxOutRef -> DecoratedTxOut -> redeemer) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoSpentScriptTx' f scriptVal red = do
    constr <- get
    let utxos   = filterScriptUtxos $ txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Map.filterWithKey f utxos
    if Map.null utxos'
        then return Nothing
        else do
            let utxo    = head $ Map.toList utxos'
                ref     = fst utxo
                val     = uncurry scriptVal utxo
                r       = Redeemer $ toBuiltinData $ uncurry red utxo
                script  = fmap getValidator val
                mutxo'  = find (\(_, o) -> _decoratedTxOutReferenceScript o == Just script) $ Map.toList utxos
                lookups = case mutxo' of
                    Nothing    -> unspentOutputs (Map.fromList [utxo]) <> otherScript val
                    Just utxo' -> unspentOutputs (Map.fromList [utxo, utxo']) <> otherScript val
                cons    = case mutxo' of
                    Nothing    -> mustSpendScriptOutput ref r
                    Just utxo' -> mustSpendScriptOutputWithReference ref r (fst utxo')
            put constr  {
                            txConstructorResult = res <&&> Just (lookups, cons),
                            txConstructorLookups = Map.delete ref utxos
                        }
            return $ Just utxo

-- This function is for datum referencing
utxoReferencedTx :: (TxOutRef -> DecoratedTxOut -> Bool) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoReferencedTx f = utxoReferencedTx' f >>= failTx "utxoReferencedTx" "No matching utxos found"

-- This function is for datum referencing
utxoReferencedTx' :: (TxOutRef -> DecoratedTxOut -> Bool) -> TransactionBuilder (Maybe (TxOutRef, DecoratedTxOut))
utxoReferencedTx' f = do
    constr <- get
    let utxos   = txConstructorLookups constr
        res     = txConstructorResult constr
        utxos'  = Map.filterWithKey f utxos
    if Map.null utxos'
        then return Nothing
        else do
            let utxo    = head $ Map.toList utxos'
                ref     = fst utxo
                lookups = unspentOutputs (Map.fromList [utxo])
                cons    = mustReferenceOutput ref
            put constr { txConstructorResult = res <&&> Just (lookups, cons) }
            return $ Just utxo

utxoProducedTx :: ToData datum => Address -> Value -> Maybe datum -> TransactionBuilder ()
utxoProducedTx addr val dat = do
    constr <- get
    let res = txConstructorResult constr
        d    = fmap (TxOutDatumHash . Datum . toBuiltinData) dat
        c    = singleton (MustPayToAddress addr d Nothing val)
    put constr { txConstructorResult = res <&&> Just (mempty, c) }

utxoProducedPublicKeyTx :: ToData datum => PubKeyHash -> Maybe StakingCredential -> Value -> Maybe datum -> TransactionBuilder ()
utxoProducedPublicKeyTx pkh skc val dat =
    let addr = Address (PubKeyCredential pkh) skc
    in utxoProducedTx addr val dat

utxoProducedScriptTx :: ToData datum => ValidatorHash -> Maybe StakingCredential -> Value -> datum -> TransactionBuilder ()
utxoProducedScriptTx vh skc val dat =
    let addr = Address (ScriptCredential vh) skc
    in utxoProducedTx addr val (Just dat)

tokensMintedTx :: ToData redeemer => Versioned MintingPolicy -> redeemer -> Value -> TransactionBuilder ()
tokensMintedTx mp red v = do
    constr <- get
    let res     = txConstructorResult constr
        -- Attempting to find a reference script
        utxos   = txConstructorLookups constr
        script  = fmap getMintingPolicy mp
        mutxo   = find (\(_, o) -> _decoratedTxOutReferenceScript o == Just script) $ Map.toList utxos
        lookups = case mutxo of
            Nothing   -> mintingPolicy mp
            Just utxo -> mintingPolicy mp <> unspentOutputs (Map.fromList [utxo])
        cons    = mustMintValueWithRedeemerAndReference (Redeemer $ toBuiltinData red) (fst <$> mutxo) v
    put constr { txConstructorResult = res <&&> Just (lookups,  cons)}

validatedInTimeIntervalTx :: POSIXTime -> POSIXTime -> TransactionBuilder ()
validatedInTimeIntervalTx startTime endTime = do
    constr <- get
    let res  = txConstructorResult constr
    put constr { txConstructorResult = res <&&> Just (mempty, mustValidateInTimeRange $ interval startTime endTime) }

validatedInSlotIntervalTx :: Slot -> Slot -> TransactionBuilder ()
validatedInSlotIntervalTx startSlot endSlot = do
    constr <- get
    let res  = txConstructorResult constr
    put constr { txConstructorResult = res <&&> Just (mempty, mustValidateInSlotRange $ interval startSlot endSlot) }

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