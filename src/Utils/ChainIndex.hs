{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Utils.ChainIndex where

import           Cardano.Api                       (FromJSON, ToJSON)
import           Data.Default                      (def)
import           Data.Map                          (Map, fromList)
import           Data.Maybe                        (catMaybes, fromJust)
import           GHC.Generics                      (Generic)
import           Ledger                            (Address, ChainIndexTxOut(..), TxOutRef, POSIXTime, AssetClass)
import           Ledger.Tx                         (TxOut(..), txOutRefId, toTxOut)
import           Plutus.ChainIndex                 (ChainIndexTx, Page(..), nextPageQuery)
import           Plutus.ChainIndex.Api             (UtxosResponse (page))
import           Plutus.Contract                   (AsContractError, Contract, txOutFromRef, currentTime)
import           Plutus.Contract.Request           (txsFromTxIds, unspentTxOutFromRef, utxoRefsAt, utxoRefsWithCurrency)
import           PlutusTx.Prelude                  hiding ((<>))
import           Prelude                           (Show, (<>))


data ChainIndexCache = ChainIndexCache {
    cacheAddress :: Address,
    cacheData    :: Map TxOutRef (ChainIndexTxOut, ChainIndexTx),
    cacheTime    :: POSIXTime
}
    deriving (Show, Generic, FromJSON, ToJSON)

-- cache validity is 30 seconds
cacheValidityPeriod :: POSIXTime
cacheValidityPeriod = 30_000

updateChainIndexCache :: AsContractError e => ChainIndexCache -> Contract w s e ChainIndexCache
updateChainIndexCache oldCache@(ChainIndexCache cAddr _ cTime) = do
    curTime <- currentTime
    if curTime - cTime <= cacheValidityPeriod
        then return oldCache
        else do
            utxos <- getUtxosAt cAddr
            ChainIndexCache cAddr utxos <$> currentTime

txOutsFromRefs :: forall w s e. (AsContractError e) => [TxOutRef] -> Contract w s e [TxOut]
txOutsFromRefs refs = map toTxOut <$> (catMaybes <$> traverse txOutFromRef refs)

----------------------------------- Custom queries -------------------------------

-- Does the same as utxosTxOutTxAt
getUtxosAt :: forall w s e. (AsContractError e) => Address -> Contract w s e (Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getUtxosAt addr = do
    utxoRefs <- foldUtxoRefsAt f [] addr
    txOuts <- traverse unspentTxOutFromRef utxoRefs
    let txIds = txOutRefId <$> utxoRefs 
    txs <- txsFromTxIds txIds
    return $ fromList $ map (\(ref, (txo, tx)) -> (ref, (fromJust txo, tx))) $ filter (isJust . fst . snd) $ zip utxoRefs $ zip txOuts txs
  where
      f acc pg = pure $ acc <> pageItems pg

-- Get all utxos containing a given currency
getUtxosWithCurrency :: forall w s e. (AsContractError e) => AssetClass -> Contract w s e (Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getUtxosWithCurrency addr = do
    utxoRefs <- foldUtxoRefsWithCurrency f [] addr
    txOuts <- traverse unspentTxOutFromRef utxoRefs
    let txIds = txOutRefId <$> utxoRefs 
    txs <- txsFromTxIds txIds
    return $ fromList $ map (\(ref, (txo, tx)) -> (ref, (fromJust txo, tx))) $ filter (isJust . fst . snd) $ zip utxoRefs $ zip txOuts txs
  where
      f acc pg = pure $ acc <> pageItems pg

-- | Fold through each 'Page's of unspent 'TxOutRef's at a given 'Address', and
-- accumulate the result.
foldUtxoRefsAt ::
    forall w s e a.
    ( AsContractError e
    )
    => (a -> Page TxOutRef -> Contract w s e a) -- ^ Accumulator function
    -> a -- ^ Initial value
    -> Address -- ^ Address which contain the UTXOs
    -> Contract w s e a
foldUtxoRefsAt f ini addr = go ini (Just def)
  where
    go acc Nothing = pure acc
    go acc (Just pq) = do
      pg <- page <$> utxoRefsAt pq addr
      newAcc <- f acc pg
      go newAcc (nextPageQuery pg)


-- | Fold through each 'Page's of unspent 'TxOutRef's with a given Currency, and
-- accumulate the result.
foldUtxoRefsWithCurrency ::
    forall w s e a.
    ( AsContractError e
    )
    => (a -> Page TxOutRef -> Contract w s e a) -- ^ Accumulator function
    -> a -- ^ Initial value
    -> AssetClass -- ^ Address which contain the UTXOs
    -> Contract w s e a
foldUtxoRefsWithCurrency f ini ac = go ini (Just def)
  where
    go acc Nothing = pure acc
    go acc (Just pq) = do
      pg <- page <$> utxoRefsWithCurrency pq ac
      newAcc <- f acc pg
      go newAcc (nextPageQuery pg)