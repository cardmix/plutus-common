{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Utils.Blockfrost where
import           Cardano.Api         (SerialiseAddress (serialiseAddress), StakeAddress, TxId (..))
import           Cardano.Api.Shelley (PoolId)
import           Data.Aeson          (FromJSON (..), withObject, (.:))
import qualified Data.Text           as T
import           Ledger              (Address)
import           Servant.API         (ToHttpApiData (..))
import           Text.Read           (readMaybe)
import           Utils.Address       (bech32ToAddress)

data AccDelegationHistoryResponse = AccDelegationHistoryResponse
    { adhrActiveEpoch :: Int
    , adhrTxHash      :: TxId
    , adhrAmount      :: Int
    , adhrPoolId      :: PoolId
    } deriving Show

instance FromJSON AccDelegationHistoryResponse where
    parseJSON = withObject "Tx delegation certificate response" $ \o -> do
        adhrActiveEpoch <- o .: "active_epoch"
        adhrTxHash      <- o .: "tx_hash"
        adhrAmount      <- o .: "amount" >>= maybe (fail "amount") pure . readMaybe 
        adhrPoolId      <- o .: "pool_id"
        pure AccDelegationHistoryResponse{..}

data TxDelegationsCertsResponse = TxDelegationsCertsResponse
    { tdcrIndex       :: Int
    , tdcrCertIndex   :: Int
    , tdcrAddress     :: Address
    , tdcrPoolId      :: PoolId
    , tdcrActiveEpoch :: Int
    } deriving Show

instance FromJSON TxDelegationsCertsResponse where
    parseJSON = withObject "Tx delegation certificate response" $ \o -> do
        tdcrIndex       <- o .: "index"
        tdcrCertIndex   <- o .: "cert_index"
        tdcrAddress     <- o .: "address" >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        tdcrPoolId      <- o .: "pool_id"
        tdcrActiveEpoch <- o .: "active_epoch"
        pure TxDelegationsCertsResponse{..}

data TxUtxoResponse = TxUtxoResponse
    { turTxHash :: TxId
    , turInputs :: [TxUtxoResponseInput]
    }

instance FromJSON TxUtxoResponse where
    parseJSON = withObject "Tx UTXOs response" $ \o -> do
        turTxHash <- o .: "hash"
        turInputs <- o .: "inputs"
        pure TxUtxoResponse{..}
 
data TxUtxoResponseInput = TxUtxoResponseInput
    { turiAddress :: Address
    } deriving Show

instance FromJSON TxUtxoResponseInput where
    parseJSON = withObject "Tx UTXOs response input" $ \o -> do
        turiAddress <- o .: "address" >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        pure TxUtxoResponseInput{..}

newtype Bf a = Bf a

deriving newtype instance Show a => Show (Bf a)

instance ToHttpApiData (Bf StakeAddress) where
    toUrlPiece (Bf addr) = serialiseAddress addr

instance ToHttpApiData (Bf TxId) where
    toUrlPiece = T.dropAround (== '\"') . T.pack . show