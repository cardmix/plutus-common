{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Types.TxConstructor where

import           Cardano.Api                      (FromJSON, ToJSON)
import           Data.Map                         (Map)
import           GHC.Generics                     (Generic)
import           Ledger                           hiding (singleton, unspentOutputs)
import           Ledger.Constraints.TxConstraints (TxConstraints)
import           Ledger.Constraints.OffChain      (ScriptLookups, typedValidatorLookups)
import           Ledger.Typed.Scripts             (TypedValidator)
import           Plutus.ChainIndex                (ChainIndexTx)
import           PlutusTx.Prelude                 hiding (mempty, Semigroup, (<$>), unless, mapMaybe, find, toList, fromInteger)
import           Prelude                          (Show, Monoid (mempty))

data TxConstructor a i o = TxConstructor {
    txCurrentTime        :: POSIXTime,
    txConstructorLookups :: Map TxOutRef (ChainIndexTxOut, ChainIndexTx),
    txConstructorResult  :: Maybe (ScriptLookups a, TxConstraints i o)
}
    deriving (Show, Generic, FromJSON, ToJSON)

newTx :: TypedValidator a -> POSIXTime -> Map TxOutRef (ChainIndexTxOut, ChainIndexTx) ->  TxConstructor a i o
newTx scriptVal ct lookups = TxConstructor ct lookups $ Just (typedValidatorLookups scriptVal, mempty)
