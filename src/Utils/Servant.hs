{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Utils.Servant where

import           Control.Arrow                     ((&&&))
import           Control.Monad.Catch               (Exception(..), throwM)
import           Control.Monad.IO.Class            (MonadIO(..))
import           Servant.Client                    (mkClientEnv, runClientM, ClientM, BaseUrl(..), Scheme(..))
import qualified Servant.Client                    as Servant
import           Network.HTTP.Client               (newManager, defaultManagerSettings, HttpException(..), Request(port), HttpExceptionContent)
import           Types.Error                       (ConnectionError(..))

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