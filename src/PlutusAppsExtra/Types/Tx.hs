{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module PlutusAppsExtra.Types.Tx where

import           Cardano.Api                      (FromJSON, ToJSON)
import           Control.Monad.State              (State, execState)
import           GHC.Generics                     (Generic)
import           Ledger.Constraints.OffChain      (ScriptLookups)
import           Ledger.Constraints.TxConstraints (TxConstraints)
import           Ledger.Typed.Scripts             (Any, ValidatorTypes (..))
import           Plutus.V2.Ledger.Api             (POSIXTime)
import           PlutusTx.Prelude                 hiding (Semigroup, fromInteger, mapMaybe, mempty, toList, unless, (<$>))
import           Prelude                          (Monoid (mempty), Show)

import           PlutusAppsExtra.Types.Error      (TxBuilderError)
import           PlutusAppsExtra.Utils.ChainIndex (MapUTXO)

data TxConstructor a i o = TxConstructor
    {
        txCurrentTime          :: POSIXTime,
        txConstructorLookups   :: MapUTXO,
        txConstructorErrors    :: [TxBuilderError],
        txConstructorResult    :: Maybe (ScriptLookups a, TxConstraints i o)
    }
    deriving (Show, Generic, FromJSON, ToJSON)

type Transaction = TxConstructor Any (RedeemerType Any) (DatumType Any)
type TransactionBuilder a = State Transaction a

mkTxConstructor :: POSIXTime -> MapUTXO -> Transaction
mkTxConstructor ct lookups = TxConstructor ct lookups [] $ Just (mempty, mempty)

selectTxConstructor :: [Transaction] -> Maybe Transaction
selectTxConstructor = find (isJust . txConstructorResult)

buildTxConstraints :: TransactionBuilder () -> Transaction ->
    Maybe (ScriptLookups Any, TxConstraints (RedeemerType Any) (DatumType Any))
buildTxConstraints builder tx = txConstructorResult $ builder `execState` tx
