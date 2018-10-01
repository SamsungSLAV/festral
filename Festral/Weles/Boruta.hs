{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

-- |Simple library for test management using Weles as testing server.
module Festral.Weles.Boruta (
    curlWorkers,
    createRequest,
    allRequests,
    Worker (..)
) where

import GHC.Generics
import Data.Aeson
import Control.Applicative
import Festral.Files
import Festral.Config
import Control.Exception
import Network.Curl
import Network.Curl.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL (pack)
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import System.Process
import System.IO
import Data.List.Split

-- |Data type describing Boruta worker
data Worker = Worker
    { workerUUID    :: String
    , state         :: String
    , groups        :: String
    , caps          :: Caps
    } deriving (Generic)

-- |Caps part of the worker
data Caps = Caps
    { addr          :: String
    , deviceType    :: String
    , uuid          :: String
    } deriving (Generic)

instance Show Caps where
    show x = "  {\n"
           ++"    \"Addr\":"          ++ show (addr x)        ++ ",\n"
           ++"    \"device_type\":"   ++ show (deviceType x)  ++ ",\n"
           ++"    \"UUID\":"          ++ show (uuid x)        ++ "\n"
           ++"  }"

instance FromJSON Caps where
    parseJSON = withObject "Caps" $ \o -> do
        addr        <- o .:? "Addr"         .!= ""
        deviceType  <- o .:? "device_type"  .!= ""
        uuid        <- o .:? "UUID"         .!= ""
        return Caps{..}
instance ToJSON Caps

instance Show Worker where
    show x = "{\n"
          ++ "  \"WorkerUUID\":"  ++ show (workerUUID x)  ++ ",\n"
          ++ "  \"State\":"       ++ show (state x)       ++ ",\n"
          ++ "  \"Groups\":"      ++ show (groups x)      ++ ",\n"
          ++ "  \"Caps\":"        ++ show (caps x)        ++ "\n"
          ++ "}"

instance FromJSON Worker where
    parseJSON = withObject "Worker" $ \o -> do
        workerUUID  <- o .: "WorkerUUID"
        state       <- o .: "State"
        groups      <- o .:? "Groups" .!= ""
        caps        <- o .: "Caps"
        return Worker{..}
instance ToJSON Worker

-- |Helper datatype for getting request ID from boruta after creating it
data ReqID = ReqID {simpleReqID :: Int} deriving Generic

instance FromJSON ReqID where
    parseJSON = withObject "ReqID" $ \o -> do
        simpleReqID <- o .: "ReqID"
        return ReqID{..}
instance ToJSON ReqID

data BorutaRequest = BorutaRequest
    { reqID     :: Int
    , reqState  :: String
    , reqCaps   :: Caps
    } deriving (Generic)

instance FromJSON BorutaRequest where
    parseJSON = withObject "BorutaRequest" $ \o -> do
        reqID       <- o .: "ID"
        reqState    <- o .: "State"
        reqCaps     <- o .: "Caps"
        return BorutaRequest{..}
instance ToJSON BorutaRequest
instance Show BorutaRequest where
    show x = "{\n"
          ++ "  \"ID\":"    ++ show (reqID x)   ++ ",\n"
          ++ "  \"State\":" ++ show (reqState x)++ ",\n"
          ++ "  \"Caps\":"  ++ show (reqCaps x) ++ "\n"
          ++ "}"

data BorutaAuth = BorutaAuth
    { sshKey    :: String
    , username  :: String
    } deriving (Generic, Show)
instance FromJSON BorutaAuth where
    parseJSON = withObject "BorutaAuth" $ \o -> do
        sshKey      <- o .: "Key"
        username    <- o .: "Username"
        return BorutaAuth{..}
instance ToJSON BorutaAuth

-- |Return device type of the given worker
workerDeviceType :: Worker -> String
workerDeviceType worker = deviceType $ caps worker

borutaAddr = do
    config <- getAppConfig
    return $ borutaIP config

-- |Returns list of workers of boruta under address declared in the application configuration file
curlWorkers :: IO [Worker]
curlWorkers = do
    addr <- borutaAddr
    (err, str) <- curlGetString (addr ++ "/api/workers") [CurlFollowLocation True]
    if err == CurlOK
        then do
            let res = decode (BL.pack str) :: Maybe [Worker]
            if isNothing res
                then return []
                else return $ fromJust res
        else return []


-- |Create request for given target for 60 minutes from now with priority 4
createRequest :: String -> IO (Maybe Int)
createRequest target = do
    time <- getCurrentTime
    let now = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time
    let afterHour = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $ addUTCTime 3600 time -- Add 1 hour (3600 seconds)
    let request = "{\\\"Deadline\\\": \\\""++ afterHour ++"\\\", \\\"ValidAfter\\\": \\\""++ now ++"\\\", \\\"Caps\\\" : { \\\"device_type\\\": \\\""++ target ++ "\\\" }, \\\"Priority\\\": 4 }"
    url <- borutaAddr
    (_, out, err, _) <- runInteractiveCommand $ "curl -sL --data \"" ++ request ++"\" http://" ++ url ++ "/api/reqs/"
    outStr <- hGetContents out
    let req = decode (BL.pack outStr) :: Maybe ReqID
    return $ simpleReqID <$> req

-- |List all requests from Boruta
allRequests :: IO [BorutaRequest]
allRequests = do
    addr <- borutaAddr
    (err, str) <- curlGetString (addr ++ "/api/reqs") [CurlFollowLocation True]
    if err == CurlOK
        then do
            let res = decode (BL.pack str) :: Maybe [BorutaRequest]
            if isNothing res
                then return []
                else return $ fromJust res
        else return []

-- |Return ssh key for session for given by name target. If this target has running session
-- it returns key for it, othervise it opens new request
getTargetKey :: String -> IO (Maybe BorutaAuth)
getTargetKey device = do
    requests <- allRequests
    let active = filter (\(BorutaRequest id state caps) -> (state == "IN PROGRESS") && (deviceType caps == device)) requests
    id <- if length active == 0
        then createRequest device
        else return $ Just $ reqID $ head active
    putStrLn $ show id
    if isNothing id
        then return Nothing
        else getKey $ fromJust id

getKey :: Int -> IO (Maybe BorutaAuth)
getKey id = do
    url <- borutaAddr
    (_, out, err, _) <- runInteractiveCommand $ "curl -sL --data \"\" http://" ++ url ++ "/api/v1/reqs/" ++ show id ++ "/acquire_worker"
    outStr <- hGetContents out
    return $ decode (BL.pack outStr)
