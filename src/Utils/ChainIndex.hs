module Utils.ChainIndex where

import qualified Data.Map as Map
import           Ledger            (TxOutRef(..), DecoratedTxOut(..))
import           Ledger.Ada        (fromValue, toValue)

filterCleanUtxos :: Map.Map TxOutRef DecoratedTxOut -> Map.Map TxOutRef DecoratedTxOut
filterCleanUtxos = Map.filter $ (\v -> toValue (fromValue v) == v) . _decoratedTxOutValue
