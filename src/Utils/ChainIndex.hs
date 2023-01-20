{-# LANGUAGE TupleSections #-}

module Utils.ChainIndex where

import           Cardano.Node.Emulator.Params (Params (..))
import           Control.Monad.Catch          (MonadThrow)
import qualified Data.Map                     as Map
import           Ledger                       (TxOutRef(..), DecoratedTxOut(..), TxOut, toTxOut)
import           Ledger.Value                 (adaOnlyValue)

import           Types.Error                  (throwEither, TxBalancingError (..))

type MapUTXO = Map.Map TxOutRef DecoratedTxOut

toCardanoUtxo :: (MonadThrow m) => Params -> MapUTXO -> m (Map.Map TxOutRef TxOut)
toCardanoUtxo params utxos = 
    let f (a, b) = (a, ) <$> throwEither UnbuildableTx (toTxOut (pNetworkId params) b)
    in Map.fromList <$> mapM f (Map.toList  utxos)

filterCleanUtxos :: MapUTXO -> MapUTXO
filterCleanUtxos = Map.filter $ (\v -> adaOnlyValue v == v) . _decoratedTxOutValue