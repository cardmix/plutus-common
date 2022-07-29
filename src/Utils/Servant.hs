module Utils.Servant where

import           Servant.Client                    (mkClientEnv, runClientM, ClientM, BaseUrl(..), Scheme(..) )
import           Network.HTTP.Client               (newManager, defaultManagerSettings)

getFromEndpoint :: ClientM a -> IO a
getFromEndpoint endpoint = do
    manager <- newManager defaultManagerSettings
    responseOrError <- runClientM 
        endpoint
        (mkClientEnv manager (BaseUrl Http "localhost" 9083 ""))
    case responseOrError of
        Left _ -> error "Error while accessing endpoint."
        Right response -> pure response
