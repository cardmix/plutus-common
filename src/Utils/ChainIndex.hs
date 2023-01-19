module Utils.ChainIndex where

import qualified Data.Map          as Map
import           Ledger            (TxOutRef(..), DecoratedTxOut(..))
import           Ledger.Value      (adaOnlyValue)

type MapUTXO = Map.Map TxOutRef DecoratedTxOut

filterCleanUtxos :: MapUTXO -> MapUTXO
filterCleanUtxos = Map.filter $ (\v -> adaOnlyValue v == v) . _decoratedTxOutValue