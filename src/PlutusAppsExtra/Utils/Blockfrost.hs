{-# LANGUAGE DeriveFunctor              #-}
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
import qualified Data.Text                     as T
import           Ledger                        (Address, AssetClass, Value)
import           Ledger.Value                  (AssetClass (..), TokenName (..), Value (..), adaToken)
import           Plutus.V1.Ledger.Api          (CurrencySymbol (..), adaSymbol, fromBuiltin, toBuiltin)
import           PlutusAppsExtra.Utils.Address (bech32ToAddress)
import qualified PlutusTx.AssocMap             as PAM
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
    , turoAmount  :: Value
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


data AssetTxsResponse = AssetTxsResponse
    { atrTxHash  :: TxId
    , atrTxIndex :: Integer
    } deriving Show

instance FromJSON AssetTxsResponse where
    parseJSON = withObject "Specific asset response" $ \o -> do
        atrTxHash  <- o .: "tx_hash"
        atrTxIndex <- o .: "tx_index"
        pure AssetTxsResponse{..}

newtype Bf a = Bf {unBf :: a}
    deriving Functor

deriving newtype instance Show a => Show (Bf a)

data BfOrder = Asc | Desc

instance FromJSON (Bf Value) where
    parseJSON = withObject "Bf Value" $ \o -> (,) <$> o .: "unit" <*> o .: "quantity" >>= \case
        (J.String "lovelace", amt) -> readAmt amt <&> (Bf . Value . PAM.singleton adaSymbol . PAM.singleton adaToken)
        (J.String txt       , amt) -> do
            let (cs, name) = T.splitAt 56 txt 
            amt' <- readAmt amt
            cs' <- maybe (fail "Currency symbol from hex") (pure . CurrencySymbol . toBuiltin) $ T.decodeHex cs
            name' <-  maybe (fail "Name from hex") (pure . TokenName . toBuiltin) $ T.decodeHex name
            pure $ Bf $ Value $ PAM.singleton cs' $ PAM.singleton name' amt'
        _                          -> mzero
        where
            readAmt =  maybe (fail "Read amount from string") pure . readMaybe . T.unpack

instance ToHttpApiData BfOrder where
    toUrlPiece = \case
        Asc  -> "asc"
        Desc -> "desc"

instance ToHttpApiData (Bf StakeAddress) where
    toUrlPiece (Bf addr) = serialiseAddress addr

instance ToHttpApiData (Bf TxId) where
    toUrlPiece = T.dropAround (== '\"') . T.pack . show

instance ToHttpApiData (Bf AssetClass) where
    toUrlPiece (Bf (AssetClass (CurrencySymbol cs, TokenName token))) = T.encodeHex $ fromBuiltin $ cs <> token
