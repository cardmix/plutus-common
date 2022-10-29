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
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module IO.ChainIndex where

import           Cardano.Api                       (FromJSON, ToJSON)
import           Control.Applicative               (Applicative(..))
import           Control.Monad.Extra               (mconcatMapM)
import           Control.Monad.IO.Class            (MonadIO(..))
import           Data.Default                      (Default (def))
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           GHC.Generics                      (Generic)
import           Ledger                            (Address, ChainIndexTxOut(..), TxOutRef (txOutRefId), POSIXTime)
import           Plutus.ChainIndex                 (ChainIndexTx, Page(..), PageQuery)
import           Plutus.ChainIndex.Api             (UtxoAtAddressRequest(..), UtxosResponse(..))
import qualified Plutus.ChainIndex.Client          as Client
import           PlutusTx.Prelude                  hiding ((<>), (<$>), pure, traverse, fmap)
import           Plutus.V1.Ledger.Address          (Address(addressCredential) )
import           Prelude                           (Show(..), IO, (<$>), (<>), traverse, fmap)
import           IO.Time                           (currentTime)
import qualified Utils.Servant                     as Servant

data ChainIndexCache = ChainIndexCache {
    cacheAddresses  :: [Address],
    cacheData       :: Map TxOutRef (ChainIndexTxOut, ChainIndexTx),
    cacheTime       :: POSIXTime
}
    deriving (Show, Generic, FromJSON, ToJSON)

getFromEndpoint :: Servant.Endpoint a
getFromEndpoint = Servant.getFromEndpointOnPort 9083

-- Cache validity is 30 seconds
cacheValidityPeriod :: POSIXTime
cacheValidityPeriod = 30_000

class HasUtxoData m where
    updateChainIndexCache :: ChainIndexCache -> m ChainIndexCache

instance MonadIO m => HasUtxoData m where
    updateChainIndexCache oldCache@(ChainIndexCache addrs _ cTime) = do
        curTime <- currentTime
        if curTime - cTime <= cacheValidityPeriod
            then return oldCache
            else do
                utxos  <- liftIO $ mconcatMapM getUtxosAt addrs
                ChainIndexCache addrs utxos <$> currentTime

----------------------------------- Chain index queries ---------------------------------

-- Get all utxos at a given address
getUtxosAt :: Address -> IO (Map TxOutRef (ChainIndexTxOut, ChainIndexTx))
getUtxosAt addr = do
  refTxOuts <- Map.toList <$> foldUtxoRefsAt f Map.empty addr
  let txIds = map (txOutRefId . fst) refTxOuts
  ciTxs <- getFromEndpoint $ Client.getTxs txIds
  pure $ Map.fromList $ zipWith (fmap . flip (,)) ciTxs refTxOuts
  where
    f acc page' = do
      let utxoRefs = pageItems page'
      txOuts <- traverse (fmap Just . unspentTxOutFromRef) utxoRefs
      let utxos = Map.fromList
                $ mapMaybe (\(ref, txOut) -> fmap (ref,) txOut)
                $ zip utxoRefs txOuts
      pure $ acc <> utxos

-- Fold through each 'Page's of unspent 'TxOutRef's at a given 'Address', and
-- accumulate the result.
foldUtxoRefsAt :: forall a.
    (a -> Page TxOutRef -> IO a) -- ^ Accumulator function
    -> a -- ^ Initial value
    -> Address -- ^ Address which contain the UTXOs
    -> IO a
foldUtxoRefsAt f ini addr = go ini (Just def)
    where
        go acc Nothing = pure acc
        go acc (Just pq) = do
            page' <- page <$> utxoRefsAt pq addr
            newAcc <- f acc page'
            go newAcc (nextPageQuery page')

unspentTxOutFromRef :: TxOutRef -> IO ChainIndexTxOut
unspentTxOutFromRef = getFromEndpoint . Client.getUnspentTxOut

-- Get the unspent transaction output references at an address.
utxoRefsAt :: PageQuery TxOutRef -> Address -> IO UtxosResponse
utxoRefsAt pageQ = 
    getFromEndpoint . Client.getUtxoSetAtAddress . UtxoAtAddressRequest (Just pageQ) . addressCredential