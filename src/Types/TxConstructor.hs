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
import           Ledger.Constraints.OffChain      (ScriptLookups)
import           Plutus.ChainIndex                (ChainIndexTx)
import           PlutusTx.Prelude                 hiding (mempty, Semigroup, (<$>), unless, mapMaybe, toList, fromInteger)
import           Prelude                          (Show, Monoid (mempty))

data TxConstructor d a i o = TxConstructor {
    txCurrentTime        :: POSIXTime,
    txCreator            :: (PaymentPubKeyHash, Maybe StakePubKeyHash),
    txInputData          :: d,
    txConstructorLookups :: Map TxOutRef (ChainIndexTxOut, ChainIndexTx),
    txConstructorResult  :: Maybe (ScriptLookups a, TxConstraints i o)
}
    deriving (Show, Generic, FromJSON, ToJSON)

mkTxConstructor :: (PaymentPubKeyHash, Maybe StakePubKeyHash) -> POSIXTime -> d -> Map TxOutRef (ChainIndexTxOut, ChainIndexTx) ->
    TxConstructor d a i o
mkTxConstructor creator ct dat lookups = TxConstructor ct creator dat lookups $ Just (mempty, mempty)

selectTxConstructor :: [TxConstructor d a i o] -> Maybe (TxConstructor d a i o)
selectTxConstructor = find (isJust . txConstructorResult)
