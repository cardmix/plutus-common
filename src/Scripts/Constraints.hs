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

import           Data.Maybe                       (fromJust)
import qualified Data.Map
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
{-# INLINABLE utxoReferenced #-}
utxoReferenced :: TxInfo -> (TxOut -> Bool) -> Bool
utxoReferenced _ _ = True

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

utxoSpentPublicKeyTx :: (TxOut -> Bool) -> TxConstructor a i o -> TxConstructor a i o
utxoSpentPublicKeyTx f (TxConstructor lookups res) = TxConstructor lookups $
        if cond then res <> Just (unspentOutputs utxos, mustSpendPubKeyOutput $ head refs) else Nothing
    where
        utxos = Data.Map.map fst lookups
        refs  = Data.Map.keys $ Data.Map.filter (f . toTxOut) utxos
        cond  = not $ null refs

utxoSpentScriptTx :: (TxOut -> Bool) -> ((TxOutRef, ChainIndexTxOut) -> Validator) -> ((TxOutRef, ChainIndexTxOut) -> Redeemer)
    -> TxConstructor a i o -> TxConstructor a i o
utxoSpentScriptTx f scriptVal red (TxConstructor lookups res) = TxConstructor lookups $
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