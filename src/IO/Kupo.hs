{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module IO.Kupo where

import           Control.Monad        ((<=<), mzero )
import           Data.Aeson           (FromJSON(..), (.:), withObject)
import qualified Data.Aeson           as J
import qualified Data.Aeson.Key       as J
import qualified Data.Aeson.KeyMap    as J
import           Data.Coerce          (coerce)
import           Data.Data            (Proxy (..))
import           Data.Functor         ((<&>))
import qualified Data.Map             as Map
import qualified Data.Text            as T
import           Ledger               (Address(..), Language(..), PaymentPubKeyHash(..), DecoratedTxOut(..), Script,
                                       TxId(..), TxOutRef(..), Value, Versioned(..), ScriptHash (..))               
import qualified Ledger.Ada           as Ada
import qualified Ledger.Value         as Value
import           Network.HTTP.Client (Request, HttpExceptionContent)
import           Plutus.V1.Ledger.Api (StakingCredential(..), Credential(..), fromBuiltin, toBuiltin)
import qualified PlutusTx.AssocMap    as PMap
import           Servant.API          (ToHttpApiData(..),Get,JSON,(:>),Capture,(:<|>)((:<|>)),QueryFlag)
import           Servant.Client       (client, ClientM)
import           Text.Hex             (decodeHex, encodeHex)
import           Types.Error          (ConnectionError)
import           Utils.Address        (bech32ToKeyHashes)
import           Utils.ChainIndex     (MapUTXO)
import           Utils.Servant        (Endpoint, pattern ConnectionErrorOnPort, getFromEndpointOnPort)

-- Get all utxos at a given address
getUtxosAt :: Address -> IO MapUTXO
getUtxosAt = fmap coerce <=< (getFromEndpointKupo . getKupoUtxosAt . Kupo)

unspentTxOutFromRef :: TxOutRef -> IO DecoratedTxOut
unspentTxOutFromRef = coerce <=< (getFromEndpointKupo . getKupoUnspentTxOutFromRef . Kupo)

getSciptByHash :: ScriptHash -> IO (Versioned Script)
getSciptByHash = fmap coerce . getFromEndpointKupo . getKupoScriptByHash . Kupo

--------------------------------------------------- Kupo API ---------------------------------------------------

type KupoAPI = GetUtxosAt :<|> UnspetTxOutFromRef :<|> GetScriptByHash

getFromEndpointKupo :: Endpoint a
getFromEndpointKupo = getFromEndpointOnPort 1442

pattern KupoConnectionError :: Request -> HttpExceptionContent -> ConnectionError
pattern KupoConnectionError req content <- ConnectionErrorOnPort 1442 req content

type GetUtxosAt         = 
    "matches" :> Capture "pattern" (Kupo Address) :> QueryFlag "unspent" :> Get '[JSON] (IO KupoUTXOs)
type UnspetTxOutFromRef = 
    "matches" :> Capture "pattern" (Kupo TxOutRef) :> Get '[JSON] (IO (SingleFromArray (Kupo DecoratedTxOut)))
type GetScriptByHash    = 
    "scripts" :> Capture "script hash" (Kupo ScriptHash) :> Get '[JSON] (Kupo (Versioned Script))

getKupoUtxosAt             :: Kupo Address    -> ClientM (IO KupoUTXOs)
getKupoUnspentTxOutFromRef :: Kupo TxOutRef   -> ClientM (IO (SingleFromArray (Kupo DecoratedTxOut)))
getKupoScriptByHash        :: Kupo ScriptHash -> ClientM (Kupo (Versioned Script))
(getKupoUtxosAt, getKupoUnspentTxOutFromRef, getKupoScriptByHash)
    = ((`getKupoUtxosAt_` True), getKupoUnspentTxOutFromRef_, getKupoScriptByHash_)
    where getKupoUtxosAt_ :<|> getKupoUnspentTxOutFromRef_ :<|> getKupoScriptByHash_ = client (Proxy @KupoAPI)

----------------------------------------------- FromJSON instances -----------------------------------------------

-- Some instances need an IO because kupo responds with a script hash instead of a script

instance FromJSON (IO KupoUTXOs) where
    parseJSON = fmap (fmap (Kupo . Map.fromList . coerce) . sequence). parseJSON @[IO KupoUTXO]

instance FromJSON (IO KupoUTXO) where
    parseJSON j = ($ j) $ withObject "Kupo UTXO" $ \o -> do
        refId  <- o .: "transaction_id" <&> TxId
        refIdX <- o .: "output_index"
        ioTxOut <- parseJSON @(IO (Kupo DecoratedTxOut)) j
        pure $ (\(Kupo txOut) -> Kupo (TxOutRef refId refIdX, txOut)) <$> ioTxOut

instance FromJSON (IO (Kupo DecoratedTxOut)) where
    parseJSON = withObject "Kupo DecoratedTxOut" $ \o -> do
        addr     <- o .: "address"
        Kupo val <- o .: "value"
        script   <- o .: "script_hash" >>= \case
            J.Null        -> pure $ pure Nothing
            J.String hash -> do
                sHash <- maybe (fail "not a hex") (pure . toBuiltin) $ decodeHex hash
                pure $ Just <$> getSciptByHash (ScriptHash sHash)
            _             -> fail "script hash"
        (PaymentPubKeyHash pkh, sc) <- maybe (fail "can not get key hashes from addr") pure $ bech32ToKeyHashes addr
        pure $ Kupo . PublicKeyDecoratedTxOut pkh sc val Nothing <$> script

instance FromJSON (Kupo (Versioned Script)) where
    parseJSON = withObject "Kupo Versioned Script" $ \o -> do
        script <- o .: "script"
        lang   <- o .: "language" >>= \case
            J.String "plutus:v1" -> pure PlutusV1
            J.String "plutus:v2" -> pure PlutusV2
            _                    -> fail "script language"
        pure $ Kupo $ Versioned script lang

instance FromJSON (Kupo Value) where
    parseJSON = withObject "Kupo Value" $ \o -> do
            coins <-  o .: "coins" <&> Ada.toValue . Ada.lovelaceOf
            assets <- o .: "assets" >>= parseAssets
            pure $ Kupo $ coins <> assets
        where
            parseAssets = fmap mconcat . traverse parseAsset . J.toList
            parseAsset (asset, amount) = Value.Value . PMap.fromList <$> do
                case span (/= '.') (J.toString asset) of
                    (cs, '.' : name) -> do
                        amount' <- parseJSON amount
                        token <- Value.TokenName      <$> toBbs name
                        cs'   <- Value.CurrencySymbol <$> toBbs cs
                        pure [(cs', PMap.singleton token amount')]
                    _ -> mzero
            toBbs = maybe (fail "not a hex") (pure . toBuiltin) . decodeHex . T.pack


-- Kupo responds with array to every request
newtype SingleFromArray a = SingleFromArray a
    deriving newtype Show

instance FromJSON (IO a) => FromJSON (IO (SingleFromArray a)) where
    parseJSON = \case
        (J.Array [o]) -> fmap SingleFromArray <$> parseJSON o
        _             -> mzero

-------------------------------------------- ToHttpApiData instances --------------------------------------------

instance ToHttpApiData (Kupo TxOutRef) where
    toUrlPiece (Kupo TxOutRef{..}) = T.pack (show txOutRefIdx <> "@") <>
        encodeHex (fromBuiltin $ getTxId txOutRefId)

instance ToHttpApiData (Kupo Address) where
    toUrlPiece (Kupo Address{..}) = T.pack $ pCred <> sCred
        where
            pCred = showCred addressCredential
            sCred = case addressStakingCredential of
                Just (StakingHash cred) -> "/" <> showCred cred
                _ -> ""
            showCred = \case
                PubKeyCredential c -> show c
                ScriptCredential c -> show c

instance ToHttpApiData (Kupo ScriptHash) where
    toUrlPiece (Kupo (ScriptHash hash)) = encodeHex $ fromBuiltin hash

------------------------------------------- Newtype to avoid orphans -------------------------------------------

newtype Kupo a = Kupo a
deriving newtype instance Show a => Show (Kupo a)

type KupoUTXOs = Kupo MapUTXO
type KupoUTXO  = Kupo (TxOutRef, DecoratedTxOut)