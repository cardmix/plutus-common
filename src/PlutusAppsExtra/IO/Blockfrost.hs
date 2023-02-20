{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ViewPatterns     #-}

module PlutusAppsExtra.IO.Blockfrost where

import           Cardano.Api                      (NetworkId (..), StakeAddress, TxId, makeStakeAddress)
import           Cardano.Api.Shelley              (PoolId)
import           Control.Applicative              ((<|>))
import           Control.Monad                    (foldM)
import           Control.Monad.Catch              (Exception (..), MonadThrow (..))
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Data.Data                        (Proxy (..))
import           Data.Foldable                    (find)
import           Data.Functor                     ((<&>))
import           Data.Maybe                       (listToMaybe)
import           Ledger                           (Address, AssetClass, CurrencySymbol, StakePubKeyHash (..), TokenName)
import           Ledger.Value                     (AssetClass (..), valueOf)
import           Network.HTTP.Client              (HttpException (..), newManager)
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           PlutusAppsExtra.Types.Error      (ConnectionError (..))
import           PlutusAppsExtra.Utils.Address    (spkhToStakeCredential)
import           PlutusAppsExtra.Utils.Blockfrost (AccDelegationHistoryResponse (..), AssetTxsResponse (..), Bf (..),
                                                   BfOrder (..), TxDelegationsCertsResponse, TxUTxoResponseOutput (..),
                                                   TxUtxoResponse (..), TxUtxoResponseInput (..))
import           Servant.API                      (Capture, Get, Header, JSON, QueryParam, (:<|>) ((:<|>)), (:>))
import           Servant.Client                   (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import qualified Servant.Client                   as Servant
import Data.Aeson (eitherDecodeFileStrict)
import Data.Text (Text)

tokenFilePath :: FilePath
tokenFilePath = "blockfrost.token"

portBf :: Int
portBf = 80

getAddressFromStakePubKeyHash :: NetworkId -> PoolId -> StakePubKeyHash -> IO (Maybe Address)
getAddressFromStakePubKeyHash net poolId spkh = runMaybeT $ do
    history <- MaybeT $ sequence $ getAccountDelegationHistory . makeStakeAddress net <$> spkhToStakeCredential spkh
    txHash <- MaybeT $ pure $ adhrTxHash <$> find ((== poolId) . adhrPoolId) history
    MaybeT $ fmap turiAddress . listToMaybe . turInputs <$> getTxUtxo txHash

getStakeAddressLastPool :: StakeAddress -> IO (Maybe PoolId)
getStakeAddressLastPool stakeAddr = fmap adhrPoolId . listToMaybe <$> getAccountDelegationHistory stakeAddr

getAddressFromStakeAddress :: StakeAddress -> IO (Maybe Address)
getAddressFromStakeAddress stakeAddr = do
    txId <- fmap adhrTxHash . listToMaybe <$> getAccountDelegationHistory stakeAddr
    maybe (pure Nothing) (fmap (fmap turiAddress . listToMaybe . turInputs) . getTxUtxo) txId

-- find tx id where address have minted specific amount of asset
verifyAsset :: CurrencySymbol -> TokenName -> Integer -> Address -> IO (Maybe TxId)
verifyAsset cs token amount addr = do
    history <- getAssetTxs cs token
    foldM (\res (atrTxHash -> txId) -> (res <|>) <$> (getTxUtxo txId <&> findOutput txId . turOutputs)) Nothing history
    where
        findOutput txId outs = const (Just txId) =<< find (\o -> turoAddress o == addr && valueOf (turoAmount o) cs token == amount) outs

--------------------------------------------------- Blockfrost API ---------------------------------------------------

getTxUtxo :: TxId -> IO TxUtxoResponse
getTxUtxo txId = getFromEndpointBF $ withBfToken $ \t -> getBfTxUtxo t $ Bf txId

getAccountDelegationHistory :: StakeAddress -> IO [AccDelegationHistoryResponse]
getAccountDelegationHistory addr = getFromEndpointBF $ withBfToken $ \t -> getBfAccDelegationHistory t (Bf addr) (Just Desc)

getAssetTxs :: CurrencySymbol -> TokenName -> IO [AssetTxsResponse]
getAssetTxs cs name = getFromEndpointBF $ withBfToken $ \t -> getBfAssetTxs t (Bf $ AssetClass (cs, name))

type BfToken = Maybe Text

withBfToken :: (BfToken -> ClientM a) -> ClientM a
withBfToken ma = liftIO (eitherDecodeFileStrict tokenFilePath) >>= either error (ma . Just)

getFromEndpointBF :: ClientM a -> IO a
getFromEndpointBF endpoint = do
    manager <- newManager tlsManagerSettings
    responseOrError <- runClientM
        endpoint
        (mkClientEnv manager (BaseUrl Http "cardano-preview.blockfrost.io"  portBf ""))
    case responseOrError of
        Left (Servant.ConnectionError (fromException -> Just (HttpExceptionRequest r c)))
                       -> throwM (ConnectionError r c)
        Left err       -> throwM err
        Right response -> pure response

type BlockfrostAPI = "api" :> "v0" :>
    (    GetAccDelegationHistory
    :<|> GetTxDelegationCerts
    :<|> GetTxUtxo
    :<|> GetAssetTxs
    )

type Auth = Header "project_id" Text

type GetAccDelegationHistory
    = Auth :> "accounts" :> Capture "Stake address" (Bf StakeAddress) :> "delegations" :> QueryParam "order" BfOrder :> Get '[JSON] [AccDelegationHistoryResponse]
type GetTxDelegationCerts
    = Auth :> "txs" :> Capture "Tx hash" (Bf TxId) :> "delegations" :> Get '[JSON] TxDelegationsCertsResponse
type GetTxUtxo
    = Auth :> "txs" :> Capture "Tx hash" (Bf TxId) :> "utxos" :> Get '[JSON] TxUtxoResponse
type GetAssetTxs
    = Auth :> "assets" :> Capture "Policy id" (Bf AssetClass) :> "transactions" :> Get '[JSON] [AssetTxsResponse]

getBfAccDelegationHistory :: BfToken -> Bf StakeAddress -> Maybe BfOrder -> ClientM [AccDelegationHistoryResponse]
getBfTxDelegationCerts    :: BfToken -> Bf TxId                          -> ClientM TxDelegationsCertsResponse
getBfTxUtxo               :: BfToken -> Bf TxId                          -> ClientM TxUtxoResponse
getBfAssetTxs             :: BfToken -> Bf AssetClass                    -> ClientM [AssetTxsResponse]

(getBfAccDelegationHistory, getBfTxDelegationCerts, getBfTxUtxo, getBfAssetTxs)
    = (getBfAccDelegationHistory_, getBfTxDelegationCerts_, getBfTxUtxo_, getBfAssetTxs_)
    where
        getBfAccDelegationHistory_
            :<|> getBfTxDelegationCerts_
            :<|> getBfTxUtxo_
            :<|> getBfAssetTxs_ = do
                client (Proxy @BlockfrostAPI)