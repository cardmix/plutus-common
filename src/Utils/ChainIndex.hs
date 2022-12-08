module Utils.ChainIndex where

import qualified Data.Map as Map
import           Ledger            (TxOutRef(..), DecoratedTxOut(..))
import           Ledger.Ada        (fromValue, toValue)
import           Plutus.ChainIndex (ChainIndexTx)

filterCleanUtxos :: Map.Map TxOutRef (DecoratedTxOut, ChainIndexTx) -> Map.Map TxOutRef (DecoratedTxOut, ChainIndexTx)
filterCleanUtxos = Map.filter $ (\v -> toValue (fromValue v) == v) . _decoratedTxOutValue . fst
