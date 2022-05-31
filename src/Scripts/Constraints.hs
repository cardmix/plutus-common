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

import qualified Data.Map
import           Data.Maybe                       (fromJust)
import           Ledger                           hiding (singleton, unspentOutputs, lookup)
import           Ledger.Constraints.TxConstraints (mustSpendPubKeyOutput, mustSpendScriptOutput, mustPayWithDatumToPubKey, mustPayWithDatumToPubKeyAddress,
                                                    mustPayToOtherScriptAddress, mustPayToOtherScript, mustValidateIn, mustMintValueWithRedeemer)
import           Ledger.Constraints.OffChain      (unspentOutputs, mintingPolicy, otherScript)
import           Ledger.Value                     (getValue)
import           Plutus.V1.Ledger.Api             (FromData(..))
import           PlutusTx.AssocMap                (Map, lookup, toList)
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, toList, fromInteger, mempty)
import           Prelude                          ((<>), mempty)

import           Types.TxConstructor

----------------------------- On-Chain -------------------------------

{-# INLINABLE utxoSpent #-}
utxoSpent :: (TxOut -> Bool) -> TxInfo -> Bool
utxoSpent f info = isJust $ find f ins
    where ins = map txInInfoResolved $ txInfoInputs info

{-# INLINABLE utxoProduced #-}
utxoProduced :: (TxOut -> Bool) -> TxInfo -> Bool
utxoProduced f info = isJust $ find f outs
    where outs = txInfoOutputs info

{-# INLINABLE utxoProducedInjectiveTxOutRef #-}
utxoProducedInjectiveTxOutRef :: forall a . (FromData a) => (a -> TxOutRef) -> (TxOut -> Bool) -> ScriptContext -> Bool
utxoProducedInjectiveTxOutRef g f ctx = isJust $ find (\o -> f o && txOutDatumHash o == dh && isJust dh) outs
    where
        info = scriptContextTxInfo ctx
        outs = txInfoOutputs info
        ownRef = case scriptContextPurpose ctx of
          Spending ref -> ref
          _            -> error ()
        dh = fmap fst $ find (maybe False (== ownRef) . fmap g . fromBuiltinData . getDatum . snd) (txInfoData info)

{-# INLINABLE utxoProducedInjectiveTokenNames #-}
utxoProducedInjectiveTokenNames :: forall a . (FromData a) => (a -> (TokenName, Integer)) -> (TxOut -> Bool) -> ScriptContext -> Bool
utxoProducedInjectiveTokenNames g f ctx = all (\p -> utxoProducedInjectiveTokenName p g f ctx) m
    where
        ownCS = ownCurrencySymbol ctx
        m     = toList $ fromMaybe (error ()) $ lookup ownCS $ getValue $ txInfoMint $ scriptContextTxInfo ctx

{-# INLINABLE utxoProducedInjectiveTokenName #-}
utxoProducedInjectiveTokenName :: forall a . (FromData a) => (TokenName, Integer) -> (a -> (TokenName, Integer)) -> (TxOut -> Bool) -> ScriptContext -> Bool
utxoProducedInjectiveTokenName p g f ctx = isJust $ find (\o -> f o && txOutDatumHash o == dh && isJust dh) outs
    where
        info = scriptContextTxInfo ctx
        outs = txInfoOutputs info
        dh = fmap fst $ find (maybe False (== p) . fmap g . fromBuiltinData . getDatum . snd) (txInfoData info)

{-# INLINABLE tokensMinted #-}
tokensMinted :: Map TokenName Integer -> ScriptContext -> Bool
tokensMinted expected ctx = actual == Just expected
    where
        ownCS  = ownCurrencySymbol ctx
        actual = lookup ownCS $ getValue $ txInfoMint $ scriptContextTxInfo ctx

{-# INLINABLE tokensBurned #-}
tokensBurned :: Map TokenName Integer -> ScriptContext -> Bool
tokensBurned expected ctx = actual == Just expected
    where
        ownCS  = ownCurrencySymbol ctx
        actual = lookup ownCS $ getValue $ negate $ txInfoMint $ scriptContextTxInfo ctx

{-# INLINABLE validatedInInterval #-}
validatedInInterval :: POSIXTime -> POSIXTime -> TxInfo -> Bool
validatedInInterval startTime endTime info = intDeclared `contains` intActual
    where
        intActual   = txInfoValidRange info
        intDeclared = interval startTime endTime

{-# INLINABLE timeToValidate #-}
timeToValidate :: POSIXTime
timeToValidate = 600_000

{-# INLINABLE validatedAround #-}
validatedAround :: POSIXTime -> TxInfo -> Bool
validatedAround time = validatedInInterval time (time + timeToValidate)

-------------------------- Off-Chain -----------------------------

utxoSpentPublicKeyTx :: (TxOut -> Bool) -> TxConstructor a i o -> TxConstructor a i o
utxoSpentPublicKeyTx f (TxConstructor lookups res) = TxConstructor lookups $
        if cond then res <> Just (unspentOutputs utxos, mustSpendPubKeyOutput $ head refs) else Nothing
    where
        utxos = Data.Map.map fst lookups
        refs  = Data.Map.keys $ Data.Map.filter (f . toTxOut) utxos
        cond  = not $ null refs

utxoSpentScriptTx :: ((TxOutRef, ChainIndexTxOut) -> Validator) -> ((TxOutRef, ChainIndexTxOut) -> Redeemer) ->
    (TxOut -> Bool) -> TxConstructor a i o -> TxConstructor a i o
utxoSpentScriptTx scriptVal red f (TxConstructor lookups res) = TxConstructor lookups $
        if cond
            then res <> Just (unspentOutputs utxos <> otherScript (scriptVal $ head utxos'), mustSpendScriptOutput (fst $ head utxos') (red $ head utxos'))
            else Nothing
    where
        utxos  = Data.Map.map fst lookups
        utxos' = Data.Map.toList $ Data.Map.filter (f . toTxOut) utxos
        cond  = not $ null utxos'

utxoProducedPublicKeyTx :: PaymentPubKeyHash -> Maybe StakePubKeyHash -> Value -> Datum -> TxConstructor a i o -> TxConstructor a i o
utxoProducedPublicKeyTx pkh skh val dat (TxConstructor lookups res) = TxConstructor lookups $
        if isJust skh
            then res <> Just (mempty, mustPayWithDatumToPubKeyAddress pkh (fromJust skh) dat val)
            else res <> Just (mempty, mustPayWithDatumToPubKey pkh dat val)

utxoProducedScriptTx :: ValidatorHash -> Maybe StakeValidatorHash -> Value -> Datum -> TxConstructor a i o -> TxConstructor a i o
utxoProducedScriptTx vh svh val dat (TxConstructor lookups res) = TxConstructor lookups $
        if isJust svh
            then res <> Just (mempty, mustPayToOtherScriptAddress vh (fromJust svh) dat val)
            else res <> Just (mempty, mustPayToOtherScript vh dat val)

tokensMintedTx :: MintingPolicy -> Redeemer -> Value -> TxConstructor a i o -> TxConstructor a i o
tokensMintedTx mp r v (TxConstructor lookups res) = TxConstructor lookups $
        res <> Just (mintingPolicy mp, mustMintValueWithRedeemer r v)


tokensBurnedTx :: MintingPolicy -> Redeemer -> Value -> TxConstructor a i o -> TxConstructor a i o
tokensBurnedTx mp r v (TxConstructor lookups res) = TxConstructor lookups $
        res <> Just (mintingPolicy mp, mustMintValueWithRedeemer r (negate v))

validatedInIntervalTx :: POSIXTime -> POSIXTime -> TxConstructor a i o -> TxConstructor a i o
validatedInIntervalTx startTime endTime (TxConstructor lookups res) = TxConstructor lookups $
        res <> Just (mempty, mustValidateIn $ interval startTime endTime)

validatedAroundTx :: POSIXTime -> TxConstructor a i o -> TxConstructor a i o
validatedAroundTx time = validatedInIntervalTx time (time + timeToValidate)