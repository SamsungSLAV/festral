{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

-- |Simple library for test management using Weles as testing server.
module Festral.Weles.Boruta (
    curlWorkers,
    createRequest,
    allRequests,
    getTargetAuth,
    execDryadConsole,
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
import System.Posix.Process
import System.IO
import System.IO.Temp
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

data Addr = Addr
    { ip    :: String
    , port  :: Int
    , zone  :: String
    } deriving (Show, Generic)

instance FromJSON Addr where
    parseJSON = withObject "Addr" $ \o -> do
        ip   <- o .: "IP"
        port <- o .: "Port"
        zone <- o .: "Zone"
        return Addr{..}
instance ToJSON Addr

data BorutaAuth = BorutaAuth
    { sshKey    :: String
    , username  :: String
    , authAddr  :: Addr
    } deriving (Generic, Show)

instance FromJSON BorutaAuth where
    parseJSON = withObject "BorutaAuth" $ \o -> do
        sshKey      <- o .: "Key"
        username    <- o .: "Username"
        authAddr    <- o .: "Addr"
        return BorutaAuth{..}
instance ToJSON BorutaAuth

-- |Return device type of the given worker
workerDeviceType :: Worker -> String
workerDeviceType worker = deviceType $ caps worker

borutaAddr = do
    config <- getAppConfig
    return $ (borutaIP config, borutaPort config)

-- |Returns list of workers of boruta under address declared in the application
-- configuration file
curlWorkers :: IO [Worker]
curlWorkers = do
    (addr, port) <- borutaAddr
    (err, str) <-
        curlGetString
            (addr ++ ":" ++ port ++ "/api/workers")
            [CurlFollowLocation True]
    return $ fromMaybe [] (decode (BL.pack str) :: Maybe [Worker])

-- |Create request for given target for 60 minutes from now with priority 4
createRequest :: String -> IO (Maybe Int)
createRequest target = do
    time <- getCurrentTime
    let now = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time
    let afterHour = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $
                    addUTCTime 3600 time -- Add 1 hour (3600 seconds)
    let request = "{\\\"Deadline\\\": \\\""
                ++ afterHour ++ "\\\", \\\"ValidAfter\\\": \\\""
                ++ now ++ "\\\", \\\"Caps\\\" : { \\\"device_type\\\": \\\""
                ++ target ++ "\\\" }, \\\"Priority\\\": 4 }"
    (addr, port) <- borutaAddr
    (_, out, err, _) <- runInteractiveCommand $
        "curl -sL --data \"" ++ request ++
        "\" http://" ++ addr ++ ":" ++ port ++ "/api/reqs/"
    outStr <- hGetContents out
    let req = decode (BL.pack outStr) :: Maybe ReqID
    return $ simpleReqID <$> req

-- |List all requests from Boruta
allRequests :: IO [BorutaRequest]
allRequests = do
    (addr, port) <- borutaAddr
    (err, str) <-
        curlGetString
            (addr ++ ":" ++ port ++ "/api/reqs")
            [CurlFollowLocation True]
    return $ fromMaybe [] (decode (BL.pack str) :: Maybe [BorutaRequest])

-- |Return ssh key for session for given by name target. If this target has
-- running session it returns key for it, othervise it opens new request
getTargetAuth :: String -> IO (Maybe BorutaAuth)
getTargetAuth device = do
    requests <- allRequests
    let active = filter
            (\(BorutaRequest id state caps) ->
                (state == "IN PROGRESS")
                && (deviceType caps == device))
            requests
    id <- if length active == 0
        then createRequest device
        else return $ Just $ reqID $ head active
    maybe (return Nothing) getKey id

getKey :: Int -> IO (Maybe BorutaAuth)
getKey id = do
    (addr, port) <- borutaAddr
    (_, out, err, _) <- runInteractiveCommand $
        "curl -sL --data \"\" http://" ++ addr ++ ":" ++ port
        ++ "/api/v1/reqs/" ++ show id ++ "/acquire_worker"
    outStr <- hGetContents out
    return $ decode (BL.pack outStr)

-- |Exec ssh session for given device specified by device_type
execDryadConsole :: String -> IO ()
execDryadConsole device = do
    auth <- getTargetAuth device
    (addr, _) <- borutaAddr
    keyFile <- maybe (return "") writeKey auth
    maybe (return ())
        (\auth -> executeFile "ssh" True
            [ (username auth) ++ "@" ++ addr
            , "-p"
            , (show $ port $ authAddr auth)
            , "-i"
            , keyFile
            ] Nothing)
        auth

writeKey auth = writeSystemTempFile "boruta-key" (sshKey auth)
