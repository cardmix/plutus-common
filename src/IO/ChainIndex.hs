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
{-# LANGUAGE PatternSynonyms            #-}
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
import           Data.Functor                      ((<&>))
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           GHC.Generics                      (Generic)
import           IO.Time                           (currentTime)
import           IO.Wallet                         (HasWallet, ownAddresses)
import           Ledger                            (Address, DecoratedTxOut(..), TxOutRef (..), POSIXTime, Ada, CardanoTx, 
                                                    getCardanoTxInputs, txOutValue, txOutAddress, getCardanoTxOutputs,
                                                    TxIn (..))
import qualified Ledger.Ada                        as Ada
import           Network.HTTP.Client               (HttpExceptionContent, Request)
import           Plutus.ChainIndex                 (ChainIndexTx, Page(..), PageQuery)
import           Plutus.ChainIndex.Api             (UtxoAtAddressRequest(..), UtxosResponse(..))
import qualified Plutus.ChainIndex.Client          as Client
import           PlutusTx.Prelude                  hiding ((<>), (<$>), pure, traverse, fmap, mapM, mconcat)
import           Plutus.V1.Ledger.Address          (Address(addressCredential) )
import           Prelude                           (Show(..), IO, (<$>), (<>), traverse, fmap, mapM, mconcat)
import           Utils.ChainIndex                  (MapUTXO)
import           Utils.Servant                     (pattern ConnectionErrorOnPort, getFromEndpointOnPort, Endpoint, ConnectionError)

----------------------------------- Chain index cache -----------------------------------

data ChainIndexCache = ChainIndexCache {
    cacheAddresses  :: [Address],
    cacheData       :: MapUTXO,
    cacheTime       :: POSIXTime
}
    deriving (Show, Generic, FromJSON, ToJSON)

newCache :: [Address] -> ChainIndexCache
newCache addresses = ChainIndexCache addresses Map.empty 0

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
                utxos <- liftIO $ mconcatMapM getUtxosAt addrs
                ChainIndexCache addrs utxos <$> currentTime

----------------------------------- Chain index queries ---------------------------------

getFromEndpointChainIndex :: Endpoint a
getFromEndpointChainIndex = getFromEndpointOnPort 9083

pattern ChainIndexConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern ChainIndexConnectionError req content <- ConnectionErrorOnPort 9083 req content

-- Get all ada at a wallet
getWalletAda :: HasWallet m => m Ada 
getWalletAda = mconcat . fmap (Ada.fromValue . _decoratedTxOutValue) . Map.elems <$> getWalletUtxos

-- Get all utxos at a wallet
getWalletUtxos :: HasWallet m => m MapUTXO
getWalletUtxos = ownAddresses >>= mapM (liftIO . getUtxosAt) <&> mconcat

-- Get all utxos at a given address
getUtxosAt :: Address -> IO MapUTXO
getUtxosAt = foldUtxoRefsAt f Map.empty
    where
        f acc page' = do
          let utxoRefs = pageItems page'
          txOuts <- traverse (fmap Just . unspentTxOutFromRef) utxoRefs
          let utxos = Map.fromList
                    $ mapMaybe (\(ref, txOut) -> fmap (ref,) txOut)
                    $ zip utxoRefs txOuts
          pure $ acc <> utxos

-- Get all utxos and txs at a given address
getUtxosTxsAt :: Address -> IO (Map TxOutRef (DecoratedTxOut, ChainIndexTx))
getUtxosTxsAt addr = do
        refTxOuts <- Map.toList <$> foldUtxoRefsAt f Map.empty addr
        let txIds = map (txOutRefId . fst) refTxOuts
        ciTxs <- getFromEndpointChainIndex $ Client.getTxs txIds
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

unspentTxOutFromRef :: TxOutRef -> IO DecoratedTxOut
unspentTxOutFromRef = getFromEndpointChainIndex . Client.getUnspentTxOut

-- Get the unspent transaction output references at an address.
utxoRefsAt :: PageQuery TxOutRef -> Address -> IO UtxosResponse
utxoRefsAt pageQ =
    getFromEndpointChainIndex . Client.getUtxoSetAtAddress . UtxoAtAddressRequest (Just pageQ) . addressCredential

getTxAdaProfit :: HasWallet m => CardanoTx -> m Ada
getTxAdaProfit tx = do
    addrs <- ownAddresses
    utxos <- getWalletUtxos
    let spentRefs = map txInRef $ getCardanoTxInputs tx
        spent = sum $ map (Ada.fromValue . _decoratedTxOutValue) $ Map.elems $ Map.filterWithKey (\ref _ -> ref `elem` spentRefs) utxos
        income = sum $ map (Ada.fromValue . txOutValue) $ filter ((`elem` addrs) . txOutAddress) $ getCardanoTxOutputs tx
    pure $ income - spent

isProfitableTx :: HasWallet m => CardanoTx -> m Bool
isProfitableTx tx = (>= 0) <$> getTxAdaProfit tx