{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module PlutusAppsExtra.Utils.Blockfrost where
import           Cardano.Api                   (SerialiseAddress (serialiseAddress), StakeAddress, TxId (..))
import           Cardano.Api.Shelley           (PoolId)
import           Control.Monad                 (mzero)
import           Data.Aeson                    (FromJSON (..), withObject, withText, (.:))
import qualified Data.Aeson                    as J
import           Data.Functor                  ((<&>))
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Ledger                        (Address, MintingPolicyHash (..))
import           Plutus.V1.Ledger.Api          (CurrencySymbol (..), adaSymbol, fromBuiltin, toBuiltin)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import           Servant.API                   (ToHttpApiData (..))
import qualified Text.Hex                      as T
import           Text.Read                     (readMaybe)

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
    { turTxHash  :: TxId
    , turInputs  :: [TxUtxoResponseInput]
    , turOutputs :: [TxUTxoResponseOutput]
    } deriving (Show, Eq)

instance FromJSON TxUtxoResponse where
    parseJSON = withObject "Tx UTXOs response" $ \o -> do
        turTxHash <- o .: "hash"
        turInputs <- o .: "inputs"
        turOutputs <- o .: "outputs" 
        pure TxUtxoResponse{..}
 
data TxUtxoResponseInput = TxUtxoResponseInput
    { turiAddress :: Address
    } deriving (Show, Eq)

instance FromJSON TxUtxoResponseInput where
    parseJSON = withObject "Tx UTXOs response input" $ \o -> do
        turiAddress <- o .: "address" >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        pure TxUtxoResponseInput{..}

data TxUTxoResponseOutput = TxUTxoResponseOutput
    { turoAddress :: Address
    , turoAmount  :: Map.Map CurrencySymbol Integer
    } deriving (Show, Eq)

instance FromJSON TxUTxoResponseOutput where
    parseJSON = withObject "Tx UTXOs response input" $ \o -> do
        turoAddress   <- o .: "address" >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        turoAmount <- mconcat . fmap unBf <$> o .: "amount"
        pure TxUTxoResponseOutput{..}

data BfMintingPolarity = BfMint | BfBurn
    deriving (Show, Eq, Enum)

instance FromJSON BfMintingPolarity where
    parseJSON = withText "Bf minting polarity" $ \case
        "minted" -> pure BfMint
        "burned" -> pure BfBurn
        _        -> mzero


data AssetHistoryResponse = AssetHistoryResponse
    { ahrTxHash          :: TxId
    , ahrMintingPolarity :: BfMintingPolarity
    , ahrAmount          :: Integer
    } deriving Show

instance FromJSON AssetHistoryResponse where
    parseJSON = withObject "Specific asset response" $ \o -> do
        ahrTxHash          <- o .: "tx_hash"
        ahrMintingPolarity <- o .: "action"
        ahrAmount          <- o .: "amount" >>= maybe mzero pure . readMaybe . T.unpack
        pure AssetHistoryResponse{..}

newtype Bf a = Bf {unBf :: a}

deriving newtype instance Show a => Show (Bf a)

data BfOrder = Asc | Desc

instance FromJSON (Bf (Map.Map CurrencySymbol Integer)) where
    parseJSON = withObject "Bf Value" $ \o -> (,) <$> o .: "unit" <*> o .: "quantity" >>= \case
        (J.String "lovelace", amt) -> toMap amt adaSymbol
        (J.String policy    , amt) -> toCs policy >>= toMap amt
        _                          -> mzero
        where
            toMap amt cs = maybe (fail "Read amount from string") pure (readMaybe $ T.unpack amt) <&> Bf . Map.singleton cs
            toCs = maybe (fail "Currency symbol from hex") (pure . CurrencySymbol . toBuiltin) . T.decodeHex

instance ToHttpApiData BfOrder where
    toUrlPiece = \case
        Asc  -> "asc"
        Desc -> "desc"

instance ToHttpApiData (Bf StakeAddress) where
    toUrlPiece (Bf addr) = serialiseAddress addr

instance ToHttpApiData (Bf TxId) where
    toUrlPiece = T.dropAround (== '\"') . T.pack . show

instance ToHttpApiData (Bf MintingPolicyHash) where
    toUrlPiece (Bf (MintingPolicyHash bbs)) = T.encodeHex $ fromBuiltin bbs
