{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module Utils.Kupo where

import           Control.Lens         ((^?))
import           Control.Monad        (mzero)
import           Data.Aeson           (FromJSON(..), (.:), withObject, withArray)
import qualified Data.Aeson           as J
import qualified Data.Aeson.Key       as J
import qualified Data.Aeson.KeyMap    as J
import           Data.Aeson.Lens      (key)
import           Data.Coerce          (coerce)
import           Data.Functor         ((<&>))
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import qualified Data.Vector          as Vector
import           Ledger               (Address(..), TxOutRef(..), DecoratedTxOut(..), PaymentPubKeyHash(..), Value, TxId(..))
import qualified Ledger.Ada           as Ada
import qualified Ledger.Value         as Value
import           Plutus.V1.Ledger.Api (StakingCredential(..), Credential(..), fromBuiltin, toBuiltin)
import qualified PlutusTx.AssocMap    as PMap
import           Servant.API          (ToHttpApiData(..))
import           Text.Hex             (decodeHex, encodeHex)
import           Utils.Address        (bech32ToKeyHashes)
import           Utils.ChainIndex     (MapUTXO)

newtype Kupo a = Kupo a
deriving newtype instance Show a => Show (Kupo a)

type KupoUTXOs = Kupo MapUTXO
type KupoUTXO  = Kupo (TxOutRef, DecoratedTxOut)

instance FromJSON KupoUTXOs where
    parseJSON = withArray "Kupo MapUTXO" $ \arr -> 
            Kupo . Map.fromList . coerce <$> parseJSON @[KupoUTXO] (J.Array $ Vector.filter isNotSpent arr)
        where
            isNotSpent v = (== J.Null) $ fromMaybe J.Null $ v ^? key "spent_at"

instance FromJSON KupoUTXO where
    parseJSON j = ($ j) $ withObject "Kupo UTXO" $ \o -> do
        refId  <- o .: "transaction_id" <&> TxId
        refIdX <- o .: "output_index"
        Kupo txOut  <- parseJSON j
        pure $ Kupo (TxOutRef refId refIdX, txOut)

instance FromJSON (Kupo DecoratedTxOut) where
    parseJSON = withObject "Kupo DecoratedTxOut" $ \o -> do
        addr     <- o .: "address"
        Kupo val <- o .: "value"
        (PaymentPubKeyHash pkh, sc) <- maybe (fail "can not get key hashes from addr") pure $ bech32ToKeyHashes addr
        pure $ Kupo $ PublicKeyDecoratedTxOut pkh sc val Nothing Nothing

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

instance FromJSON a => FromJSON (SingleFromArray a) where
    parseJSON = \case
        (J.Array [o]) -> SingleFromArray <$> parseJSON o
        _             -> mzero

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