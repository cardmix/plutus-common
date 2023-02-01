{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TupleSections    #-}

module IO.Kupo where

import           Control.Monad        ((<=<) )
import           Data.Coerce          (coerce)
import           Data.Data            (Proxy (..))
import qualified Data.Map             as Map
import           Ledger               (Address(..), DecoratedTxOut(..), Script, TxOutRef(..), Versioned(..), ScriptHash(..),
                                       DatumHash(..), Datum (..), ValidatorHash (..), Validator(..))
import           Network.HTTP.Client  (Request, HttpExceptionContent)
import           Servant.API          (Get,JSON,(:>),Capture,(:<|>)((:<|>)),QueryFlag)
import           Servant.Client       (client, ClientM)
import           Types.Error          (ConnectionError)
import           Utils.ChainIndex     (MapUTXO)
import           Utils.Kupo           (KupoUTXOs, Kupo(..), KupoDecoratedTxOut(..))
import           Utils.Servant        (Endpoint, pattern ConnectionErrorOnPort, getFromEndpointOnPort)
import Data.Maybe (listToMaybe)

-- Get all utxos at a given address
getUtxosAt :: Address -> IO MapUTXO
getUtxosAt = fromKupoUtxos <=< (getFromEndpointKupo . getKupoUtxosAt . Kupo)

unspentTxOutFromRef :: TxOutRef -> IO (Maybe DecoratedTxOut)
unspentTxOutFromRef = sequence . listToMaybe . fmap fromKupoDecoratedTxOut <=< 
    (getFromEndpointKupo . getKupoUnspentTxOutFromRef . Kupo)

getSciptByHash :: ScriptHash -> IO (Versioned Script)
getSciptByHash = fmap coerce . getFromEndpointKupo . getKupoScriptByHash . Kupo

getValidatorByHash :: ValidatorHash -> IO (Versioned Validator)
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
    "scripts" :> Capture "script hash" (Kupo ScriptHash) :> Get '[JSON] (Kupo (Versioned Script))
type GetValidatorByHash =
    "scripts" :> Capture "validator hash" (Kupo ValidatorHash) :> Get '[JSON] (Kupo (Versioned Validator))
type GetDatumByHash     =
    "datums"  :> Capture "datum hash" (Kupo DatumHash) :> Get '[JSON] (Kupo Datum)

getKupoUtxosAt             :: Kupo Address       -> ClientM KupoUTXOs
getKupoUnspentTxOutFromRef :: Kupo TxOutRef      -> ClientM [KupoDecoratedTxOut]
getKupoScriptByHash        :: Kupo ScriptHash    -> ClientM (Kupo (Versioned Script))
getKupoValidatorByHash     :: Kupo ValidatorHash -> ClientM (Kupo (Versioned Validator))
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
        _decoratedTxOutValidator       <- Just <$> getValidatorByHash _decoratedTxOutValidatorHash
        pure ScriptDecoratedTxOut{..}
    where
        getScript = maybe (pure Nothing) (fmap Just . getSciptByHash)
        getDatum (dh, dtype) = (dh,) . dtype <$> getDatumByHash dh