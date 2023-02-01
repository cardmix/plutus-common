{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE LambdaCase #-}

module Utils.Servant where

import           Control.Arrow             ((&&&))
import           Control.Monad.Catch       (Exception (..), MonadCatch, handle, throwM)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Network.HTTP.Client       (HttpException (..), HttpExceptionContent (..), Request (port), defaultManagerSettings,
                                            newManager)
import           Network.HTTP.Types.Status (Status (statusCode))
import           Servant.Client            (BaseUrl (..), ClientM, Scheme (..), mkClientEnv, runClientM)
import qualified Servant.Client            as Servant
import           Types.Error               (ConnectionError (..))

type Endpoint a = forall m. MonadIO m => ClientM a -> m a

getFromEndpointOnPort :: Int -> Endpoint a
getFromEndpointOnPort p endpoint = liftIO $ do
    manager <- newManager defaultManagerSettings
    responseOrError <- runClientM 
        endpoint
        (mkClientEnv manager (BaseUrl Http "localhost" p ""))
    case responseOrError of
        Left (Servant.ConnectionError (fromException -> Just (HttpExceptionRequest r c))) 
                       -> throwM (ConnectionError r c) 
        Left err       -> throwM err 
        Right response -> pure response

pattern ConnectionErrorOnPort :: Int -> Request -> HttpExceptionContent -> ConnectionError
pattern ConnectionErrorOnPort port req c <- ConnectionError (id &&& port -> (req, port)) c

handle404 :: MonadCatch m => m a -> m a -> m a
handle404 h = handle $ \case
    Servant.FailureResponse _ ((statusCode . Servant.responseStatusCode -> 404)) -> h
    e                                                                            -> throwM e