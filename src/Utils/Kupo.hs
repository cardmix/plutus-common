{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module Utils.Kupo where

import           Codec.Serialise      (deserialise)
import           Control.Monad        (mzero )
import           Data.Aeson           (FromJSON(..), (.:), withObject)
import qualified Data.Aeson           as J
import qualified Data.Aeson.Key       as J
import qualified Data.Aeson.KeyMap    as J
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce          (coerce)
import           Data.Functor         ((<&>))
import qualified Data.Text            as T
import           Ledger               (Address(..), Language(..), Script,
                                       TxId(..), TxOutRef(..), Value, Versioned(..), ScriptHash(..), DatumHash(..),
                                       DatumFromQuery(..), Datum (..), ValidatorHash (..), Validator(..), PubKeyHash)
import qualified Ledger.Ada           as Ada
import qualified Ledger.Value         as Value
import           Plutus.V1.Ledger.Api (StakingCredential(..), Credential(..), fromBuiltin, toBuiltin)
import qualified PlutusTx.AssocMap    as PMap
import           PlutusTx.Builtins    (BuiltinByteString)
import           Servant.API          (ToHttpApiData(..))
import           Text.Hex             (decodeHex, encodeHex)
import           Utils.Address        (bech32ToAddress)

------------------------------------------- Newtype to avoid orphans -------------------------------------------

newtype Kupo a = Kupo a

type KupoUTXO  = Kupo (TxOutRef, KupoDecoratedTxOut)
type KupoUTXOs = [KupoUTXO]

data KupoDecoratedTxOut
    = KupoPublicKeyDecoratedTxOut
        { _decoratedTxOutPubKeyHash          :: PubKeyHash
        , _decoratedTxOutStakingCredential   :: Maybe StakingCredential
        , _decoratedTxOutValue               :: Value
        , _decoratedTxOutPubKeyDatumHash     :: Maybe (DatumHash, Datum -> DatumFromQuery)
        , _decoratedTxOutReferenceScriptHash :: Maybe ScriptHash
        }
    | KupoScriptDecoratedTxOut
        { _decoratedTxOutValidatorHash       :: ValidatorHash
        , _decoratedTxOutStakingCredential   :: Maybe StakingCredential
        , _decoratedTxOutValue               :: Value
        , _decoratedTxOutScriptDatum         :: (DatumHash, Datum -> DatumFromQuery)
        , _decoratedTxOutReferenceScriptHash :: Maybe ScriptHash
        }

----------------------------------------------- FromJSON instances -----------------------------------------------

instance FromJSON KupoUTXO where
    parseJSON j = ($ j) $ withObject "Kupo UTXO" $ \o -> do
        refId  <- o .: "transaction_id" <&> TxId
        refIdX <- o .: "output_index"
        txOut <- parseJSON j
        pure $ Kupo (TxOutRef refId refIdX, txOut)

instance FromJSON KupoDecoratedTxOut where
    parseJSON = withObject "KupoDecoratedTxOut" $ \o -> do
        addr      <- (o .: "address") >>= maybe (fail "bech32ToAddress") pure . bech32ToAddress
        Kupo val  <- o .: "value"
        datumHash <- o .: "datum_hash" >>= \case
            J.Null        -> pure Nothing
            J.String hash -> Just . DatumHash <$> toBbs hash
            _             -> fail "datum hash"
        datum <- case datumHash of
            Nothing -> pure Nothing
            Just dh  -> o .: "datum_type" >>= \case
                J.String "hash"   -> pure $ Just (dh, DatumInBody)
                J.String "inline" -> pure $ Just (dh, DatumInline)
                _                 -> fail "datum type"
        let sc = addressStakingCredential addr
        script    <- o .: "script_hash" >>= \case
            J.Null      -> pure Nothing
            J.String sh -> Just . ScriptHash <$> toBbs sh
            _           -> fail "script hash"
        case addressCredential addr of
            PubKeyCredential pkh -> pure $ KupoPublicKeyDecoratedTxOut pkh sc val datum script
            ScriptCredential vh -> do
                datum' <- maybe (fail "script txOut without datum") pure datum
                pure $ KupoScriptDecoratedTxOut vh sc val datum' script
        where toBbs = maybe (fail "not a hex") (pure . toBuiltin) . decodeHex

instance FromJSON (Kupo (Versioned Script)) where
    parseJSON = withObject "Kupo Versioned Script" $ \o -> do
        script <- o .: "script"
        lang   <- o .: "language" >>= \case
            J.String "plutus:v1" -> pure PlutusV1
            J.String "plutus:v2" -> pure PlutusV2
            _                    -> fail "script language"
        pure $ Kupo $ Versioned script lang

instance FromJSON (Kupo (Versioned Validator)) where
    parseJSON = withObject "Kupo Versioned Validator" $ \o -> do
        validator <- Validator <$> o .: "script"
        lang   <- o .: "language" >>= \case
            J.String "plutus:v1" -> pure PlutusV1
            J.String "plutus:v2" -> pure PlutusV2
            _                    -> fail "script language"
        pure $ Kupo $ Versioned validator lang

instance FromJSON (Kupo Datum) where
    parseJSON = withObject "Kupo Datum" $ \o -> o .: "datum" >>=
        maybe (fail "not a hex") (pure . Kupo . Datum . deserialise . LBS.fromStrict) . decodeHex

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
                        name'   <- Value.TokenName      <$> toBbs name
                        cs'     <- Value.CurrencySymbol <$> toBbs cs
                        pure [(cs', PMap.singleton  name' amount')]
                    _ -> mzero
            toBbs = maybe (fail "not a hex") (pure . toBuiltin) . decodeHex . T.pack


-- Kupo responds with array to every request
newtype SingleFromArray a = SingleFromArray a
    deriving newtype Show

instance FromJSON a => FromJSON (SingleFromArray a) where
    parseJSON = \case
        (J.Array [o]) -> SingleFromArray <$> parseJSON o
        _             -> fail "not a single element array"

-------------------------------------------- ToHttpApiData instances --------------------------------------------

instance ToHttpApiData (Kupo TxOutRef) where
    toUrlPiece (Kupo TxOutRef{..}) = T.pack (show txOutRefIdx <> "@") <>
        encodeHex (fromBuiltin $ getTxId txOutRefId)

instance ToHttpApiData (Kupo Address) where
    toUrlPiece (Kupo Address{..}) = T.pack $ pCred <> "/" <> sCred
        where
            pCred = showCred addressCredential
            sCred = case addressStakingCredential of
                Just (StakingHash cred) -> showCred cred
                _                       -> "*"
            showCred = \case
                PubKeyCredential c -> show c
                ScriptCredential c -> show c

instance ToHttpApiData (Kupo ScriptHash) where
    toUrlPiece = encodeHex . fromBuiltin @BuiltinByteString . coerce

deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo ValidatorHash)
deriving via (Kupo ScriptHash) instance ToHttpApiData (Kupo DatumHash)