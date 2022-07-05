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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Contracts.ChainIndex where

import           Cardano.Api                       (FromJSON, ToJSON)
import           Control.Monad.Extra               (mconcatMapM)
import           Data.Map                          (Map, union)
import           GHC.Generics                      (Generic)
import           Ledger                            (Address, ChainIndexTxOut(..), TxOutRef, POSIXTime, CurrencySymbol)
import           Plutus.ChainIndex                 (ChainIndexTx)
import           PlutusTx.Prelude                  hiding ((<>), (<$>))
import           Prelude                           (Show, undefined, IO, (<$>))


data ChainIndexCache = ChainIndexCache {
    cacheAddresses  :: [Address],
    cacheCurrencies :: [CurrencySymbol],
    cacheData       :: Map TxOutRef (ChainIndexTxOut, ChainIndexTx),
    cacheTime       :: POSIXTime
}
    deriving (Show, Generic, FromJSON, ToJSON)

-- cache validity is 30 seconds
cacheValidityPeriod :: POSIXTime
cacheValidityPeriod = 30_000

updateChainIndexCache :: ChainIndexCache -> IO ChainIndexCache
updateChainIndexCache oldCache@(ChainIndexCache addrs curs _ cTime) = do
    curTime <- currentTime
    if curTime - cTime <= cacheValidityPeriod
        then return oldCache
        else do
            utxos'  <- mconcatMapM getUtxosAt addrs
            utxos'' <- mconcatMapM getUtxosWithCurrency curs
            let utxos = utxos' `union` utxos''
            ChainIndexCache addrs curs utxos <$> currentTime

currentTime :: IO POSIXTime
currentTime = undefined

----------------------------------- Chain index queries ---------------------------------

-- Get all utxos at a given address
getUtxosAt :: Address -> IO (Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getUtxosAt _ = undefined

-- Get all utxos containing a given currency
getUtxosWithCurrency :: CurrencySymbol -> IO (Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getUtxosWithCurrency _ = undefined
