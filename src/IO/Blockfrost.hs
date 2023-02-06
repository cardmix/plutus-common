{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ViewPatterns     #-}

module IO.Blockfrost where

import           Cardano.Api               (NetworkId (..), StakeAddress, TxId, makeStakeAddress)
import           Cardano.Api.Shelley       (PoolId)
import           Control.Applicative       ((<|>))
import           Control.Monad             (foldM)
import           Control.Monad.Catch       (Exception (..), MonadThrow (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Data                 (Proxy (..))
import           Data.Foldable             (find)
import           Data.Functor              ((<&>))
import qualified Data.Map                  as Map
import           Data.Maybe                (listToMaybe)
import           Ledger                    (Address, MintingPolicyHash (..), StakePubKeyHash (..))
import           Ledger.Value              (CurrencySymbol (CurrencySymbol))
import           Network.HTTP.Client       (HttpException (..), newManager)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Servant.API               (Capture, Get, Header, JSON, QueryParam, (:<|>) ((:<|>)), (:>))
import           Servant.Client            (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import qualified Servant.Client            as Servant
import           Types.Error               (ConnectionError (..))
import           Utils.Address             (spkhToStakeCredential)
import           Utils.Blockfrost          (AccDelegationHistoryResponse (..), AssetHistoryResponse (..), Bf (..),
                                            BfMintingPolarity (..), BfOrder (..), TxDelegationsCertsResponse,
                                            TxUTxoResponseOutput (..), TxUtxoResponse (..), TxUtxoResponseInput (..))

tokenFilePath :: FilePath
tokenFilePath = "testnet/preview/blockfrost.token"

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
verifyAsset :: MintingPolicyHash -> Integer -> Address -> IO (Maybe TxId)
verifyAsset mp@(MintingPolicyHash h) amount addr = do
    history <- filter (\AssetHistoryResponse{..} -> ahrMintingPolarity == BfMint && ahrAmount == amount) <$> getAssetHistory mp
    foldM (\res (ahrTxHash -> txId) -> pure res <|> (getTxUtxo txId <&> findOutput txId . turOutputs)) Nothing history
    where
        findOutput txId outs = const (Just txId) =<< find (\o -> turoAddress o == addr &&  Map.lookup cs (turoAmount o) == Just amount) outs
        cs = CurrencySymbol h

--------------------------------------------------- Blockfrost API ---------------------------------------------------

getTxUtxo :: TxId -> IO TxUtxoResponse
getTxUtxo txId = getFromEndpointBF $ withBfToken $ \t -> getBfTxUtxo t $ Bf txId

getAccountDelegationHistory :: StakeAddress -> IO [AccDelegationHistoryResponse]
getAccountDelegationHistory addr = getFromEndpointBF $ withBfToken $ \t -> getBfAccDelegationHistory t (Bf addr) (Just Desc)

getAssetHistory :: MintingPolicyHash -> IO [AssetHistoryResponse]
getAssetHistory mph = getFromEndpointBF $ withBfToken $ \t -> getBfAssetHistory t (Bf mph)

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
    :<|> GetAssetHistory
    )

type Auth = Header "project_id" String

type GetAccDelegationHistory
    = Auth :> "accounts" :> Capture "Stake address" (Bf StakeAddress) :> "delegations" :> QueryParam "order" BfOrder :> Get '[JSON] [AccDelegationHistoryResponse]
type GetTxDelegationCerts
    = Auth :> "txs" :> Capture "Tx hash" (Bf TxId) :> "delegations" :> Get '[JSON] TxDelegationsCertsResponse
type GetTxUtxo
    = Auth :> "txs" :> Capture "Tx hash" (Bf TxId) :> "utxos" :> Get '[JSON] TxUtxoResponse
type GetAssetHistory
    = Auth :> "assets" :> Capture "Policy id" (Bf MintingPolicyHash) :> "history" :> Get '[JSON] [AssetHistoryResponse]

getBfAccDelegationHistory :: BfToken -> Bf StakeAddress      -> Maybe BfOrder -> ClientM [AccDelegationHistoryResponse]
getBfTxDelegationCerts    :: BfToken -> Bf TxId                               -> ClientM TxDelegationsCertsResponse
getBfTxUtxo               :: BfToken -> Bf TxId                               -> ClientM TxUtxoResponse
getBfAssetHistory         :: BfToken -> Bf MintingPolicyHash                  -> ClientM [AssetHistoryResponse]

(getBfAccDelegationHistory, getBfTxDelegationCerts, getBfTxUtxo, getBfAssetHistory)
    = (getBfAccDelegationHistory_, getBfTxDelegationCerts_, getBfTxUtxo_, getBfAssetHistory_)
    where
        getBfAccDelegationHistory_
            :<|> getBfTxDelegationCerts_
            :<|> getBfTxUtxo_
            :<|> getBfAssetHistory_ = do
                client (Proxy @BlockfrostAPI)