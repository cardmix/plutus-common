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

module Types.Error where

import           Cardano.Api                      (FromJSON, ToJSON)
import           Control.Exception                (Exception)
import           Control.Monad.Catch              (MonadThrow (..))
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Network.HTTP.Client              (Request, HttpExceptionContent)
import           Prelude                          

data ConnectionError = ConnectionError Request HttpExceptionContent
    deriving (Show, Exception)

data TxBuilderError = TxBuilderError
    {
        txBuilderErrorIn     :: Text,
        txBuilderErrorReason :: Text
    }
    deriving (Show, Exception, Eq, Generic, FromJSON, ToJSON)

data MkTxError = UnbuildableTx
    deriving (Show, Exception, Eq, Generic, FromJSON, ToJSON)

throwEither :: (MonadThrow m, Exception e) => e -> Either b a -> m a
throwEither e = either (const $ throwM e) pure