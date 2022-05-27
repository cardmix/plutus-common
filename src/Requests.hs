{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}


module Requests (activateRequest, endpointRequest, statusRequest, awaitStatusUpdate, stopRequest) where

import           Control.Concurrent                       (threadDelay)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Data.Aeson
import           Data.Proxy                               (Proxy (..))
import           Data.Text                                (Text, pack)
import           Data.UUID                                (UUID)
import           Network.HTTP.Req
import           Plutus.PAB.Events.Contract               (ContractInstanceId(..))
import           Plutus.PAB.Events.ContractInstanceState  (observableState)
import           Plutus.PAB.Webserver.Types               (ContractInstanceClientState(..), ContractActivationArgs (..))
import           Prelude
import           Wallet.Emulator.Wallet                   (Wallet)

import           PABContracts                             (PABContracts)

------------------------------- API Requests -------------------------------------

-- Activate a contract for a given wallet
activateRequest :: Text -> PABContracts -> Maybe Wallet -> IO UUID
activateRequest ip x w = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http ip /: "api" /: "contract" /: "activate")
        (ReqBodyJson $ ContractActivationArgs x w)
        (Proxy :: Proxy (JsonResponse ContractInstanceId))
        (port 9080)
    return $ unContractInstanceId $ responseBody v

-- Call an endpoint
endpointRequest :: (ToJSON p, Show p) => Text -> Text -> UUID -> p -> IO ()
endpointRequest ip endp uuid x = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http ip /: "api"  /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: endp)
        (ReqBodyJson x)
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Request processed: " ++ show x
        else "Error processing request!"

statusRequest :: (FromJSON p) => Text -> UUID -> IO (Maybe p)
statusRequest ip uuid = runReq defaultHttpConfig $ do
    v <- req
        GET
        (http ip /: "api" /: "contract" /: "instance" /: pack (show uuid) /: "status")
        NoReqBody
        (Proxy :: Proxy (JsonResponse (ContractInstanceClientState PABContracts)))
        (port 9080)
    let x = fromJSON $ observableState $ cicCurrentState $ responseBody v
    case x of
        Success tt -> return (Just tt)
        Error _    -> return Nothing

awaitStatusUpdate :: FromJSON a => Text -> UUID -> IO a
awaitStatusUpdate ip cid = do
    resp <- statusRequest ip cid
    case resp of
        Just state -> return state
        Nothing    -> do
                    threadDelay 1_000_000
                    awaitStatusUpdate ip cid

stopRequest :: Text -> UUID -> IO ()
stopRequest ip uuid = runReq defaultHttpConfig $ do
    v <- req
        PUT
        (http ip /: "api" /: "contract" /: "instance" /: pack (show uuid) /: "stop")
        NoReqBody
        (Proxy :: Proxy (JsonResponse ()))
        (port 9080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Request processed!"
        else "Error processing request!"