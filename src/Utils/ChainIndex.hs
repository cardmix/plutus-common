module Utils.ChainIndex where

import qualified Data.Map as Map
import           Ledger            (ChainIndexTxOut(..), TxOutRef(..))
import           Ledger.Ada        (fromValue, toValue)
import           Plutus.ChainIndex (ChainIndexTx)

filterCleanUtxos :: Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx) -> Map.Map TxOutRef (ChainIndexTxOut, ChainIndexTx)
filterCleanUtxos = Map.filter $ (\v -> toValue (fromValue v) == v) . _ciTxOutValue . fst