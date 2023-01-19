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
import           Control.Monad.State              (State, execState)
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Ledger                           (StakingCredential)
import           Ledger.Address                   (PaymentPubKeyHash)
import           Ledger.Constraints.TxConstraints (TxConstraints)
import           Ledger.Constraints.OffChain      (ScriptLookups)
import           Ledger.Typed.Scripts             (ValidatorTypes (..), Any)
import           Plutus.V2.Ledger.Api             (POSIXTime)
import           PlutusTx.Prelude                 hiding (Semigroup, (<$>), mempty, unless, mapMaybe, toList, fromInteger)
import           Prelude                          (Show, Monoid (mempty))
import qualified Prelude                          as Haskell
import           Utils.ChainIndex                 (MapUTXO)

data TxConstructorError = TxConstructorError
    {
        txConstructorErrorIn     :: Text,
        txConstructorErrorReason :: Text
    }
    deriving (Show, Haskell.Eq, Generic, FromJSON, ToJSON)

data TxConstructor a i o = TxConstructor
    {
        txCreator              :: (PaymentPubKeyHash, Maybe StakingCredential),
        txCurrentTime          :: POSIXTime,
        txConstructorLookups   :: MapUTXO,
        txConstructorErrors    :: [TxConstructorError],
        txConstructorResult    :: Maybe (ScriptLookups a, TxConstraints i o)
    }
    deriving (Show, Generic, FromJSON, ToJSON)

type Transaction = TxConstructor Any (RedeemerType Any) (DatumType Any)
type TransactionBuilder a = State Transaction a

mkTxConstructor :: (PaymentPubKeyHash, Maybe StakingCredential) -> POSIXTime -> MapUTXO -> Transaction
mkTxConstructor creator ct lookups = TxConstructor creator ct lookups [] $ Just (mempty, mempty)

selectTxConstructor :: [Transaction] -> Maybe Transaction
selectTxConstructor = find (isJust . txConstructorResult)

constructTxConstraints :: TransactionBuilder () -> Transaction ->
    Maybe (ScriptLookups Any, TxConstraints (RedeemerType Any) (DatumType Any))
constructTxConstraints builder tx = txConstructorResult $ builder `execState` tx