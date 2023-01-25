{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module IO.Kupo where
    
import           Data.Coerce         (coerce)
import           Data.Data           (Proxy (..))
import           Ledger              (Address(..), TxOutRef (..), DecoratedTxOut (..))
import           Network.HTTP.Client (Request, HttpExceptionContent)
import           Servant.API         (Get, JSON, (:>), Capture, (:<|>) ((:<|>)))
import           Servant.Client      (client, ClientM)
import           Types.Error         (ConnectionError)
import           Utils.ChainIndex    (MapUTXO)
import           Utils.Kupo          (KupoUTXOs, Kupo(..), SingleFromArray(..))
import           Utils.Servant       (Endpoint, pattern ConnectionErrorOnPort, getFromEndpointOnPort)

-- Get all utxos at a given address
getUtxosAt :: Address -> IO MapUTXO
getUtxosAt = fmap coerce . getFromEndpointKupo . getKupoUtxosAt . Kupo

unspentTxOutFromRef :: TxOutRef -> IO DecoratedTxOut
unspentTxOutFromRef = fmap coerce . getFromEndpointKupo . getKupoUnspentTxOutFromRef . Kupo

type KupoAPI = GetUtxosAt :<|> UnspetTxOutFromRef

getFromEndpointKupo :: Endpoint a
getFromEndpointKupo = getFromEndpointOnPort 1442

pattern KupoConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern KupoConnectionError req content <- ConnectionErrorOnPort 1442 req content

type KupoEndpoint arg res = "matches" :>  Capture "" (Kupo arg) :> Get '[JSON] res
type GetUtxosAt         = KupoEndpoint Address  KupoUTXOs
type UnspetTxOutFromRef = KupoEndpoint TxOutRef (SingleFromArray (Kupo DecoratedTxOut))
    
getKupoUtxosAt :: Kupo Address -> ClientM KupoUTXOs
getKupoUnspentTxOutFromRef :: Kupo TxOutRef -> ClientM (SingleFromArray (Kupo DecoratedTxOut))
(getKupoUtxosAt, getKupoUnspentTxOutFromRef) = (getKupoUtxosAt_, getKupoUnspentTxOutFromRef_)
    where getKupoUtxosAt_ :<|> getKupoUnspentTxOutFromRef_ = client (Proxy @KupoAPI)