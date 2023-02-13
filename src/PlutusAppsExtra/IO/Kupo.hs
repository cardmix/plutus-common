{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module PlutusAppsExtra.IO.Kupo where

import           Control.Monad                    ((<=<), join)
import           Data.Coerce                      (coerce)
import           Data.Data                        (Proxy (..))
import qualified Data.Map                         as Map
import           Data.Maybe                       (listToMaybe)
import           Ledger                           (Address (..), Datum (..), DatumHash (..), DecoratedTxOut (..), Script,
                                                   ScriptHash (..), TxOutRef (..), Validator (..), ValidatorHash (..),
                                                   Versioned (..))
import           Network.HTTP.Client              (HttpExceptionContent, Request)
import           PlutusAppsExtra.Types.Error      (ConnectionError)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)
import           PlutusAppsExtra.Utils.Kupo       (Kupo (..), KupoDecoratedTxOut (..), KupoUTXOs)
import           PlutusAppsExtra.Utils.Servant    (Endpoint, getFromEndpointOnPort, pattern ConnectionErrorOnPort)
import           Servant.API                      (Capture, Get, JSON, QueryFlag, (:<|>) ((:<|>)), (:>))
import           Servant.Client                   (ClientM, client)

-- Get all utxos at a given address
getUtxosAt :: Address -> IO MapUTXO
getUtxosAt = fromKupoUtxos <=< (getFromEndpointKupo . getKupoUtxosAt . Kupo)

unspentTxOutFromRef :: TxOutRef -> IO (Maybe DecoratedTxOut)
unspentTxOutFromRef = sequence . listToMaybe . fmap fromKupoDecoratedTxOut <=<
    (getFromEndpointKupo . getKupoUnspentTxOutFromRef . Kupo)

getSciptByHash :: ScriptHash -> IO (Maybe (Versioned Script))
getSciptByHash = fmap coerce . getFromEndpointKupo . getKupoScriptByHash . Kupo

getValidatorByHash :: ValidatorHash -> IO (Maybe (Versioned Validator))
getValidatorByHash = fmap coerce . getFromEndpointKupo . getKupoValidatorByHash . Kupo

getDatumByHash :: DatumHash -> IO Datum
getDatumByHash = fmap coerce . getFromEndpointKupo . getKupoDatumByHash . Kupo

--------------------------------------------------- Kupo API ---------------------------------------------------

type KupoAPI = GetUtxosAt :<|> UnspetTxOutFromRef :<|> GetScriptByHash :<|> GetValidatorByHash :<|> GetDatumByHash

getFromEndpointKupo :: Endpoint a
getFromEndpointKupo = getFromEndpointOnPort 1442

pattern KupoConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern KupoConnectionError req content <- ConnectionErrorOnPort 1442 req content

type GetUtxosAt         =
    "matches" :> Capture "pattern" (Kupo Address) :> QueryFlag "unspent" :> Get '[JSON] KupoUTXOs
type UnspetTxOutFromRef =
    "matches" :> Capture "pattern" (Kupo TxOutRef) :> QueryFlag "unspent" :> Get '[JSON] [KupoDecoratedTxOut]
type GetScriptByHash    =
    "scripts" :> Capture "script hash" (Kupo ScriptHash) :> Get '[JSON] (Maybe (Kupo (Versioned Script)))
type GetValidatorByHash =
    "scripts" :> Capture "validator hash" (Kupo ValidatorHash) :> Get '[JSON] (Maybe (Kupo (Versioned Validator)))
type GetDatumByHash     =
    "datums"  :> Capture "datum hash" (Kupo DatumHash) :> Get '[JSON] (Kupo Datum)

getKupoUtxosAt             :: Kupo Address       -> ClientM KupoUTXOs
getKupoUnspentTxOutFromRef :: Kupo TxOutRef      -> ClientM [KupoDecoratedTxOut]
getKupoScriptByHash        :: Kupo ScriptHash    -> ClientM (Maybe (Kupo (Versioned Script)))
getKupoValidatorByHash     :: Kupo ValidatorHash -> ClientM (Maybe (Kupo (Versioned Validator)))
getKupoDatumByHash         :: Kupo DatumHash     -> ClientM (Kupo Datum)
(getKupoUtxosAt, getKupoUnspentTxOutFromRef, getKupoScriptByHash, getKupoValidatorByHash, getKupoDatumByHash)
    = ((`getKupoUtxosAt_` True)
      ,(`getKupoUnspentTxOutFromRef_` True)
      ,getKupoScriptByHash_
      ,getKupoValidatorByHash_
      ,getKupoDatumByHash_
      )
    where
        getKupoUtxosAt_
            :<|> getKupoUnspentTxOutFromRef_
            :<|> getKupoScriptByHash_
            :<|> getKupoValidatorByHash_
            :<|> getKupoDatumByHash_ = client (Proxy @KupoAPI)

fromKupoUtxos :: KupoUTXOs -> IO MapUTXO
fromKupoUtxos = fmap Map.fromList . mapM (mapM fromKupoDecoratedTxOut . coerce)

fromKupoDecoratedTxOut :: KupoDecoratedTxOut -> IO DecoratedTxOut
fromKupoDecoratedTxOut = \case
    KupoPublicKeyDecoratedTxOut{..} -> do
        _decoratedTxOutPubKeyDatum     <- maybe (pure Nothing) (fmap Just . getDatum) _decoratedTxOutPubKeyDatumHash
        _decoratedTxOutReferenceScript <- getScript _decoratedTxOutReferenceScriptHash
        pure PublicKeyDecoratedTxOut{..}
    KupoScriptDecoratedTxOut{..} -> do
        _decoratedTxOutScriptDatum     <- getDatum  _decoratedTxOutScriptDatum
        _decoratedTxOutReferenceScript <- getScript _decoratedTxOutReferenceScriptHash
        _decoratedTxOutValidator       <- getValidatorByHash _decoratedTxOutValidatorHash
        pure ScriptDecoratedTxOut{..}
    where
        getScript = fmap join . mapM getSciptByHash
        getDatum (dh, dtype) = (dh,) . dtype <$> getDatumByHash dh