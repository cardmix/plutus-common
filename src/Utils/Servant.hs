module Utils.Servant where

import           Control.Exception                 (throw)
import           Servant.Client                    (mkClientEnv, runClientM, ClientM, BaseUrl(..), Scheme(..) )
import           Network.HTTP.Client               (newManager, defaultManagerSettings)

type Endpoint a = ClientM a -> IO a

getFromEndpointOnPort :: Int -> Endpoint a
getFromEndpointOnPort port endpoint = do
    manager <- newManager defaultManagerSettings
    responseOrError <- runClientM 
        endpoint
        (mkClientEnv manager (BaseUrl Http "localhost" port ""))
    case responseOrError of
        Left err       -> throw err -- "Error while accessing endpoint."
        Right response -> pure response