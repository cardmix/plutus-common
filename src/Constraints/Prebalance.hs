{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Constraints.Prebalance where

import           Data.Functor                     (($>))
import           Data.List                        (minimumBy)
import qualified Data.Map
import           Ledger                           (DecoratedTxOut (_decoratedTxOutValue), _decoratedTxOutAddress, toPubKeyHash)
import           Ledger.Address                   (PaymentPubKeyHash (..), stakingCredential)
import           Ledger.Value                     (geq, flattenValue)
import           Plutus.V2.Ledger.Api
import           PlutusTx.Prelude                 hiding (Semigroup(..), (<$>), unless, toList, fromInteger, mempty)
import           Prelude                          (Show)

import           Constraints.OffChain             (utxoSpentPublicKeyTx, utxoProducedPublicKeyTx, failTx)
import           Types.Tx                         (TransactionBuilder)

data PrebalanceConstraints = PrebalanceConstraints
    {
        consMaxExternalUtxosInTx    :: Integer,
        consMaxExternalUtxosInReq   :: Integer,
        consMaxTokensInExternalUtxo :: Integer,
        minimalAdaValue             :: Value
    }
    deriving (Show)

prebalanceTx :: PrebalanceConstraints -> Data.Map.Map TxOutRef DecoratedTxOut -> Value -> TransactionBuilder (Maybe ())
prebalanceTx cons extUtxos maxVal = prebalanceTx' cons extUtxos maxVal >>=
    failTx "prebalanceTx" "Cannot satisfy balancing constraints"

prebalanceTx' :: PrebalanceConstraints ->
                -- external wallet UTXOs that can be used for balancing
                Data.Map.Map TxOutRef DecoratedTxOut ->
                -- maximal combined value of external UTXOs that can be consumed (must have at least that value in external UTXOs)
                Value ->
                TransactionBuilder (Maybe ())
prebalanceTx' (PrebalanceConstraints n k m v) extUtxos maxVal = do
    let llst = filter (\l -> listValue l `geq` (maxVal + v)) $ sublistsBounded n $ take k $
            Data.Map.toList $ Data.Map.filter (\o -> length (flattenValue $ _decoratedTxOutValue o) <= m) extUtxos
    if null llst
        then return Nothing
        else do
            let lst = minimumBy listOrdering llst
            if null lst
                then return $ Just ()
                else do
                    let addr      = _decoratedTxOutAddress $ snd $ head lst
                        valChange = listValue lst - maxVal
                    mapM_ (\(ref, _) -> utxoSpentPublicKeyTx (\r _ -> r == ref)) lst
                    fromMaybe (failTx "prebalanceTx" "External utxos contain a script utxo" Nothing $> ()) $ do
                        pkh <- toPubKeyHash addr
                        return $ utxoProducedPublicKeyTx (PaymentPubKeyHash pkh) (stakingCredential addr) valChange (Nothing :: Maybe ())
                    return $ Just ()

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = let next = sublists xs in next ++ map (x:) next

sublistsBounded :: Integer -> [a] -> [[a]]
sublistsBounded n lst = filter ((<= n) . length) (sublists lst)

listValue :: [(TxOutRef, DecoratedTxOut)] -> Value
listValue = sum . map (_decoratedTxOutValue . snd)

listTokenSize :: [(TxOutRef, DecoratedTxOut)] -> Integer
listTokenSize = length . flattenValue . listValue

listOrdering :: [(TxOutRef, DecoratedTxOut)] -> [(TxOutRef, DecoratedTxOut)] -> Ordering
listOrdering lst1 lst2
    | listTokenSize lst1 > listTokenSize lst2 = GT
    | listTokenSize lst1 < listTokenSize lst2 = LT
    | otherwise                               = EQ
