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
{-# LANGUAGE OverloadedStrings          #-}

module Types.Error where

import           Cardano.Api                      (FromJSON, ToJSON)
import           Cardano.Wallet.Api.Types         (ApiSerialisedTransaction)
import           Cardano.Wallet.Primitive.Types   (WalletId)
import           Control.Exception                (Exception)
import           Control.Monad.Catch              (MonadThrow (..))
import qualified Data.Aeson                       as J
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Ledger                           (CardanoTx, Address)
import           Network.HTTP.Client              (Request, HttpExceptionContent)
import           Prelude                          

data ConnectionError = ConnectionError Request HttpExceptionContent
    deriving (Show, Exception)

instance ToJSON ConnectionError where
    toJSON _ = J.String "Connection error."

data TxBuilderError = TxBuilderError
    {
        txBuilderErrorIn     :: Text,
        txBuilderErrorReason :: Text
    }
    deriving (Show, Exception, Eq, Generic, FromJSON, ToJSON)

data MkTxError 
    = AllConstructorsFailed 
    | CantExtractHashFromCardanoTx CardanoTx
    | CantExtractKeyHashesFromAddress Address
    | CantExtractTxOutRefsFromEmulatorTx
    | ConvertApiSerialisedTxToCardanoTxError ApiSerialisedTransaction
    | ConvertCardanoTxToSealedTxError CardanoTx
    | UnbuildableTxOut
    | UnbuildableExportTx
    | UnbuildableUnbalancedTx
    deriving (Show, Exception, Eq, Generic, FromJSON, ToJSON)

data BalanceExternalTxError 
    = MakeUnbalancedTxError
    | MakeBuildTxFromEmulatorTxError
    | NonBabbageEraChangeAddress
    | MakeUtxoProviderError
    | MakeAutoBalancedTxError
    deriving (Show, Exception, Eq, Generic, FromJSON, ToJSON)

data WalletError
    = RestoredWalletParsingError Text
    | UnparsableAddress Text
    | WalletIdDoesntHaveAnyAssociatedAddresses WalletId
    | AddressDoesntCorrespondToPubKey Address
    deriving (Show, Exception)

data SubmitTxToLocalNodeError
    = CantSubmitEmulatorTx CardanoTx
    deriving (Show, Exception)

throwMaybe :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
throwMaybe e = maybe (throwM e) pure 

throwEither :: (MonadThrow m, Exception e) => e -> Either b a -> m a
throwEither e = either (const $ throwM e) pure
