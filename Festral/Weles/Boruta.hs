{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

-- |Simple library for test management using Weles as testing server.
module Festral.Weles.Boruta (
    curlWorkers,
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
           ++"    \"DeviceType\":"    ++ show (deviceType x)  ++ ",\n"
           ++"    \"UUID\":"          ++ show (uuid x)        ++ ",\n"
           ++"  }"

instance FromJSON Caps where
    parseJSON = withObject "Caps" $ \o -> do
        addr        <- o .:? "Addr" .!= ""
        deviceType  <- o .: "DeviceType"
        uuid        <- o .: "UUID"
        return Caps{..}
instance ToJSON Caps

instance Show Worker where
    show x = "{\n"
          ++ "  \"WorkerUUID\":"  ++ show (workerUUID x)  ++ ",\n"
          ++ "  \"State\":"       ++ show (state x)       ++ ",\n"
          ++ "  \"Groups\":"      ++ show (groups x)      ++ ",\n"
          ++ "  \"Caps\":"        ++ show (caps x)        ++ ",\n"
          ++ "}"

instance FromJSON Worker where
    parseJSON = withObject "Worker" $ \o -> do
        workerUUID  <- o .: "WorkerUUID"
        state       <- o .: "State"
        groups      <- o .:? "Groups" .!= ""
        caps        <- o .: "Caps"
        return Worker{..}
instance ToJSON Worker

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
