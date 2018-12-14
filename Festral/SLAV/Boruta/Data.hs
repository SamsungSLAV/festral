{-
 - Copyright (c) 2018 Samsung Electronics Co., Ltd All Rights Reserved
 -
 - Author: Uladzislau Harbuz <u.harbuz@samsung.com>
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -      http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License
 -}

{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

-- |Module contains data types used by "Boruta" module.
module Festral.SLAV.Boruta.Data
    ( Worker          (..)
    , BorutaRequest   (..)
    , Caps            (..)
    , BorutaAuth      (..)
    , ReqID           (..)
    , DryadSSH        (..)
    , WorkerState     (..)
    , Addr            (..)
    ) where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
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
    , deviceType    :: String -- ^Field for back compatibility with older Weles
    , device_type   :: String
    , uuid          :: String
    } deriving (Generic)

-- |Helper datatype for getting request ID from boruta after creating it
data ReqID = ReqID {simpleReqID :: Int} deriving Generic

-- |Requests (recieved and sent) for Boruta
data BorutaRequest
    -- |Responce recieved from Boruta. Contains ID of the created request and
    -- its state.
    = BorutaRequestIn
    { reqID     :: Int
    , reqState  :: String
    , reqCapsIn :: Caps
    }
    -- |Request to be send for Boruta when ask it about creating new request. It
    -- contains needed data: priority, needed times and information about
    -- requested device.
    | BorutaRequestOut
    { deadline  :: String
    , validAfter:: String
    , reqCapsOut:: Caps
    , priority  :: Int
    }
    deriving (Generic)

-- |Representation of the responce of the Boruta which gives information where
-- to connect to the requested device.
data Addr = Addr
    { ip    :: String
    , port  :: Int
    , zone  :: String
    } deriving (Show, Generic)

-- |Responce from Boruta server for request about connection to the opened
-- 'BorutaRequest'. It contains SSH key, username and address for connecting
-- to the MuxPi specified by 'reqID'.
data BorutaAuth = BorutaAuth
    { sshKey    :: String
    , username  :: String
    , authAddr  :: Addr
    } deriving (Generic, Show)

data DryadSSH = DryadSSH
    { dsUser    :: String
    , dsIp      :: String
    , dsPort    :: Int
    , idFile    :: FilePath
    }

-- |Helper type for send JSON with WorkerState to the Boruta.
data WorkerState = WorkerState String deriving Generic

instance ToJSON WorkerState where
    toJSON (WorkerState state) = object ["WorkerState" .= state]

instance FromJSON Addr where
    parseJSON = withObject "Addr" $ \o -> do
        ip   <- o .: "IP"
        port <- o .: "Port"
        zone <- o .: "Zone"
        return Addr{..}

instance ToJSON Addr

instance FromJSON BorutaRequest where
    parseJSON = withObject "BorutaRequestIn" $ \o -> do
        reqID       <- o .: "ID"
        reqState    <- o .: "State"
        reqCapsIn   <- o .: "Caps"
        return BorutaRequestIn{..}

instance ToJSON BorutaRequest where
    toJSON (BorutaRequestOut d v caps p) =
        object ["Deadline"      .= d
               ,"ValidAfter"    .= v
               ,"Priority"      .= p
               ,"Caps"          .= caps
               ]

instance Show BorutaRequest where
    show (BorutaRequestIn i s c) =
          "[" ++ show i ++ "," ++ s ++ "," ++ BL.unpack (encode c) ++ "]\n"
    show x = show $ toJSON x

instance Show Caps where
    show x = "  {\n"
           ++"    \"Addr\":"          ++ show (addr x)        ++ ",\n"
           ++"    \"device_type\":"   ++
                if (device_type x == "")
                    then deviceType x
                    else device_type x
           ++ ",\n"
           ++"    \"UUID\":"          ++ show (uuid x)        ++ "\n"
           ++"  }"

instance FromJSON Caps where
    parseJSON = withObject "Caps" $ \o -> do
        addr        <- o .:? "Addr"         .!= ""
        deviceType  <- o .:? "DeviceType"   .!= ""
        device_type  <- o .:? "device_type" .!= ""
        uuid        <- o .:? "UUID"         .!= ""
        return Caps{..}

instance ToJSON Caps where
    toJSON (Caps a old_d d u) = object $ catMaybes
        [ "Addr"        .=. a
        , "device_type" .=. if d == "" then old_d else d
        , "UUID"        .=. u
        ]

-- |Helper function which  allow to make JSON with only not empty fields.
(.=.) x y = if y == "" then Nothing else Just (x .= y)

instance FromJSON ReqID where
    parseJSON = withObject "ReqID" $ \o -> do
        simpleReqID <- o .: "ReqID"
        return ReqID{..}
instance ToJSON ReqID

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

instance FromJSON BorutaAuth where
    parseJSON = withObject "BorutaAuth" $ \o -> do
        sshKey      <- o .: "Key"
        username    <- o .: "Username"
        authAddr    <- o .: "Addr"
        return BorutaAuth{..}
instance ToJSON BorutaAuth
