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


module Types.Tx where

import           Cardano.Api                      (FromJSON, ToJSON)
import           Control.Monad.State              (State)
import           Data.Map                         (Map)
import           GHC.Generics                     (Generic)
import           Ledger                           (DecoratedTxOut)
import           Ledger.Address                   (PaymentPubKeyHash, StakePubKeyHash)
import           Ledger.Constraints.TxConstraints (TxConstraints)
import           Ledger.Constraints.OffChain      (ScriptLookups)
import           Ledger.Typed.Scripts             (ValidatorTypes (..), Any)
import           Plutus.V2.Ledger.Api             (POSIXTime, TxOutRef)
import           PlutusTx.Prelude                 hiding (mempty, Semigroup, (<$>), unless, mapMaybe, toList, fromInteger)
import           Prelude                          (Show, Monoid (mempty))


data TxConstructor a i o = TxConstructor
    {
        txCurrentTime        :: POSIXTime,
        txCreator            :: (PaymentPubKeyHash, Maybe StakePubKeyHash),
        txConstructorLookups :: Map TxOutRef DecoratedTxOut,
        txConstructorResult  :: Maybe (ScriptLookups a, TxConstraints i o)
    }
    deriving (Show, Generic, FromJSON, ToJSON)

type Transaction = TxConstructor Any (RedeemerType Any) (DatumType Any)
type TransactionBuilder a = State Transaction a

mkTxConstructor :: (PaymentPubKeyHash, Maybe StakePubKeyHash) -> POSIXTime -> Map TxOutRef DecoratedTxOut ->
    TxConstructor a i o
mkTxConstructor creator ct lookups = TxConstructor ct creator lookups $ Just (mempty, mempty)

selectTxConstructor :: [TxConstructor a i o] -> Maybe (TxConstructor a i o)
selectTxConstructor = find (isJust . txConstructorResult)