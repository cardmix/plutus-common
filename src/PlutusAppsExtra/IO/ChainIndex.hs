{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module PlutusAppsExtra.IO.ChainIndex where

import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Aeson                           (FromJSON)
import           GHC.Generics                         (Generic)
import           Ledger                               (Address, TxOutRef, DecoratedTxOut)
import qualified PlutusAppsExtra.IO.ChainIndex.Kupo   as Kupo
import qualified PlutusAppsExtra.IO.ChainIndex.Plutus as Plutus
import           PlutusAppsExtra.Utils.ChainIndex     (MapUTXO)

data ChainIndex = Plutus | Kupo
    deriving (Show, Generic, FromJSON)

class MonadIO m => HasChainIndex m where
    getChainIndex :: m ChainIndex

getUtxosAt :: HasChainIndex m => Address -> m MapUTXO
getUtxosAt addr = getChainIndex >>= \case
    Plutus -> liftIO $ Plutus.getUtxosAt addr
    Kupo   -> liftIO $   Kupo.getUtxosAt addr

getUnspentTxOutFromRef :: HasChainIndex m => TxOutRef -> m (Maybe DecoratedTxOut)
getUnspentTxOutFromRef txOutRef = getChainIndex >>= \case
    Plutus -> liftIO $ Plutus.getUnspentTxOutFromRef txOutRef
    Kupo   -> liftIO $   Kupo.getUnspentTxOutFromRef txOutRef