{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Utils.Servant where

import           Control.Exception                 (throw, Exception (fromException))
import           Control.Monad.IO.Class            (MonadIO(..))
import           Servant.Client                    (mkClientEnv, runClientM, ClientM, BaseUrl(..), Scheme(..), ClientError(..))
import           Network.HTTP.Client               (newManager, defaultManagerSettings, HttpException(..), Request(port))

type Endpoint a = forall m. MonadIO m => ClientM a -> m a

getFromEndpointOnPort :: Int -> Endpoint a
getFromEndpointOnPort p endpoint = liftIO $ do
    manager <- newManager defaultManagerSettings
    responseOrError <- runClientM 
        endpoint
        (mkClientEnv manager (BaseUrl Http "localhost" p ""))
    case responseOrError of
        Left err       -> throw err -- "Error while accessing endpoint."
        Right response -> pure response

pattern ConnectionErrorOnPort :: Int -> ClientError
pattern ConnectionErrorOnPort port 
    <- ConnectionError (fromException -> Just (HttpExceptionRequest (port -> port) _))