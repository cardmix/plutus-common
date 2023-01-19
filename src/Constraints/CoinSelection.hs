{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Constraints.CoinSelection where

import           Data.Bool                        (bool)
import qualified Data.Map                         as Map
import           Ledger                           (DecoratedTxOut (..))
import           Ledger.Value                     (geq)
import           Plutus.V2.Ledger.Api
import qualified PlutusTx.Prelude                 as Plutus
import           Prelude
import           System.Random                    (RandomGen, Random (..), mkStdGen)

import           Utils.ChainIndex                 (MapUTXO)

-- TODO: implement budget constraints
-- TODO: make use of the coin selection library from cardano-wallet

data CoinSelectionBudget = CoinSelectionBudget
    {
        utxosNumberBudget :: Integer,
        utxoBytesBudget   :: Integer
    }
    deriving (Eq, Show)

newtype CoinSelectionParams = CoinSelectionParams
    {
        valueToPay :: Value
    }

type CoinSelection = MapUTXO

genCoinSelection :: CoinSelectionBudget -> CoinSelectionParams -> CoinSelection -> Maybe CoinSelection
genCoinSelection budget params s = bool Nothing (Just $ fst $ shrinkCoinSelection budget params (s, g)) checkParams
    where
        checkParams = checkCoinSelectionParams params s
        g = mkStdGen 101245

shrinkCoinSelection :: RandomGen gen => CoinSelectionBudget -> CoinSelectionParams -> (CoinSelection, gen) -> (CoinSelection, gen)
shrinkCoinSelection budget params (s, g) = bool (s, g') (shrinkCoinSelection budget params (s', g')) $ checkCoinSelectionParams params s'
    where
        (k, g') = randomR (0, length (Map.toList s) - 1) g
        s' = Map.deleteAt k s

checkCoinSelectionBudget :: CoinSelectionBudget -> CoinSelection -> Bool
checkCoinSelectionBudget _ _ = True

checkCoinSelectionParams :: CoinSelectionParams -> CoinSelection -> Bool
checkCoinSelectionParams params s = Plutus.sum (Map.elems $ Map.map  _decoratedTxOutValue s) `geq` valueToPay params