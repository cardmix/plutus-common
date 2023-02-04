{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ViewPatterns     #-}

module IO.Blockfrost where

import           Cardano.Api             (NetworkId (..), StakeAddress, TxId, makeStakeAddress)
import           Cardano.Api.Shelley     (PoolId)
import           Control.Monad.Catch     (Exception (..), MonadThrow (..))
import           Control.Monad.IO.Class  (MonadIO (..))
import           Data.Data               (Proxy (..))
import           Data.Foldable           (find)
import           Data.Maybe              (listToMaybe)
import           Ledger                  (Address, StakePubKeyHash (..))
import           Network.HTTP.Client     (HttpException (..), newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API             (Capture, Get, Header, JSON, (:<|>) ((:<|>)), (:>), QueryParam)
import           Servant.Client          (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import qualified Servant.Client          as Servant
import           Types.Error             (ConnectionError (..))
import           Utils.Address           (spkhToStakeCredential)
import           Utils.Blockfrost        (AccDelegationHistoryResponse (..), Bf (..), TxDelegationsCertsResponse,
                                          TxUtxoResponse (..), TxUtxoResponseInput (..), BfOrder (..))

tokenFilePath :: FilePath
tokenFilePath = "testnet/preview/blockfrost.token"

portBf :: Int
portBf = 80

getAddressFromStakePubKeyHash :: NetworkId -> PoolId -> StakePubKeyHash -> IO (Maybe Address)
getAddressFromStakePubKeyHash net poolId spkh = do
    stakeAddr <- maybe (error "") (pure . makeStakeAddress net) $ spkhToStakeCredential spkh
    history   <- getAccountDelegationHistory stakeAddr
    txHash    <- maybe (error "") (pure . adhrTxHash) $ find ((== poolId) . adhrPoolId) history
    fmap turiAddress . listToMaybe . turInputs <$> getTxUtxo txHash

getStakeAddressLastPool :: StakeAddress -> IO (Maybe PoolId)
getStakeAddressLastPool stakeAddr = fmap adhrPoolId . listToMaybe <$> getAccountDelegationHistory stakeAddr

getAddressFromStakeAddress :: StakeAddress -> IO (Maybe Address)
getAddressFromStakeAddress stakeAddr = do
    txId <- fmap adhrTxHash . listToMaybe <$> getAccountDelegationHistory stakeAddr
    maybe (pure Nothing) (fmap (fmap turiAddress . listToMaybe . turInputs) . getTxUtxo) txId

--------------------------------------------------- Blockfrost API ---------------------------------------------------

getTxUtxo :: TxId -> IO TxUtxoResponse
getTxUtxo txId = getFromEndpointBF $ withBfToken $ \t -> getBfTxUtxo t $ Bf txId

getAccountDelegationHistory :: StakeAddress -> IO [AccDelegationHistoryResponse]
getAccountDelegationHistory addr = getFromEndpointBF $ withBfToken $ \t -> getBfAccDelegationHistory t (Bf addr) (Just Desc)

type BfToken = Maybe String

withBfToken :: (BfToken -> ClientM a) -> ClientM a
withBfToken ma = liftIO (readFile tokenFilePath) >>= ma . Just

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
    )

type Auth = Header "project_id" String

type GetAccDelegationHistory
    = Auth :> "accounts" :> Capture "Stake address" (Bf StakeAddress) :> "delegations" :> QueryParam "order" BfOrder :> Get '[JSON] [AccDelegationHistoryResponse]
type GetTxDelegationCerts
    = Auth :> "txs" :> Capture "Tx hash" (Bf TxId) :> "delegations" :> Get '[JSON] TxDelegationsCertsResponse
type GetTxUtxo
    = Auth :> "txs" :> Capture "Tx hash" (Bf TxId) :> "utxos" :> Get '[JSON] TxUtxoResponse

getBfAccDelegationHistory :: BfToken -> Bf StakeAddress -> Maybe BfOrder -> ClientM [AccDelegationHistoryResponse]
getBfTxDelegationCerts    :: BfToken -> Bf TxId         -> ClientM TxDelegationsCertsResponse
getBfTxUtxo               :: BfToken -> Bf TxId         -> ClientM TxUtxoResponse

(getBfAccDelegationHistory, getBfTxDelegationCerts, getBfTxUtxo)
    = (getBfAccDelegationHistory_, getBfTxDelegationCerts_, getBfTxUtxo_)
    where
        getBfAccDelegationHistory_
            :<|> getBfTxDelegationCerts_
            :<|> getBfTxUtxo_ = do
                client (Proxy @BlockfrostAPI)