{-
 - Copyright (c) 2019 Samsung Electronics Co., Ltd All Rights Reserved
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- |Module with data types used by "Weles" module.
module Festral.SLAV.Weles.Data
    ( Job             (..)
    , JobParameters   (..)
    , APIVersion      (..)
    , Artifact        (..)
    , ArtifactFilter  (..)
    , SimpleJob       (..)
    , emptyArtifact
    , (<||>)
    , (<|||>)
) where

import Data.Aeson
import GHC.Generics
import Control.Applicative
import Data.Maybe

-- |Job datatype describes json job object got from weles
data Job = Job
    {jobid      :: Int
    ,name       :: String
    ,created    :: String
    ,updated    :: String
    ,status     :: String
    ,info       :: String
} deriving (Generic)

-- |Version of the Weles API.
data APIVersion = APIVersion
    { version   :: String
    , server    :: String
    , state     :: String
    } deriving (Generic, Show)

-- |Representation of the Weles Job's artifact
data Artifact = Artifact
    { a_id      :: Int
    , a_path    :: FilePath
    , a_status  :: String
    , a_time    :: String
    , a_alias   :: String
    , a_jobid   :: Int
    , a_type    :: String
    , a_uri     :: String
    } deriving (Generic, Show)

-- |Helper type for filtering 'Artifact' got from Weles.
data ArtifactFilter = ArtifactFilter
    { a_filter  :: Artifact
    } deriving (Generic, Show)

-- |Datatype incapsulating parameters of the job
data JobParameters
    = JobParameters {
    -- |Timeout of job from it was created. Job will be closed after it expired
    -- even if it just waiting all the time. It is in seconds.
      absoluteTimeout   :: Int
    -- |Timeout which starts from job finished waiting and started running. It
    -- has no sence to be longer than 'absoluteTimeout' value because
    -- 'absoluteTimeout' has higher priority. It is in seconds.
    , afterRunTimeout   :: Int
    }

-- |Helper type for extracting JobId from JSON.
data SimpleJob = SimpleJob {s_jobid :: Int}
    deriving (Show)

instance FromJSON SimpleJob where
    parseJSON = withObject "SimpleJob" $ \v -> SimpleJob <$> v.: "jobid"

instance FromJSON APIVersion where
    parseJSON = withObject "APIVersion" $ \o -> do
        version <- o .:?    "API"   .!= "0"
        server  <- o .:?    "Server".!= ""
        state   <- o .:?    "State" .!= "deprecated"
        return APIVersion{..}

instance Show JobParameters where
    show x =  "TTL: " ++ show (absoluteTimeout x)
           ++ " RunTTL: " ++ show (afterRunTimeout x)

instance FromJSON Job where
    parseJSON = withObject "Job" $ \o -> do
        jobid   <- o .:     "jobid"     <|> o .: "jobID"
        name    <- o .:?    "name"      .!= ""
        created <- o .:?    "created"   .!= ""
        updated <- o .:?    "updated"   .!= ""
        status  <- o .:     "status"
        info    <- o .:?    "info"      .!= ""
        return Job{..}

instance ToJSON Job where
    toJSON Job{..} =
        object ["jobID"     .= jobid
               ,"name"      .= name
               ,"created"   .= created
               ,"updated"   .= updated
               ,"status"    .= status
               ,"info"      .= info
               ]

instance Show Job where
    show (Job id n c u s i) = "{\n \"jobid\" : " ++ show id ++ ",\n \"name\" : "
                ++ show n ++ ",\n \"created\" : "
                ++ show c ++ ",\n \"updated\" : "
                ++ show u ++ ",\n \"status\" : "
                ++ show s ++ ",\n \"info\" : "
                ++ show i ++ "\n}\n"

instance FromJSON Artifact where
    parseJSON = withObject "Artifact" $ \o -> do
        a_id    <- o .:     "ID"
        a_path  <- o .:?    "Path"      .!= ""
        a_status<- o .:?    "Status"    .!= ""
        a_time  <- o .:?    "Timestamp" .!= ""
        a_alias <- o .:?    "Alias"     .!= ""
        a_jobid <- o .:     "JobID"
        a_type  <- o .:?    "Type"      .!= ""
        a_uri   <- o .:?    "URI"       .!= ""
        return Artifact{..}

instance ToJSON Artifact where
    toJSON Artifact{..} = object $ catMaybes
        [ "ID"      .=! a_id
        , "Alias"   .=. a_alias
        , "JobID"   .=! a_jobid
        , "Status"  .=. a_status
        , "Type"    .=. a_type
        ]

instance ToJSON ArtifactFilter where
    toJSON ArtifactFilter{..} = object ["Filter" .= a_filter]

(.=.) x y = if y == "" then Nothing else Just (x .= [y])
(.=!) x y = if y == 0 then Nothing else Just (x .= [y])

-- |Create empty 'Artifact' record, this record
emptyArtifact = Artifact 0 "" "" "" "" 0 "" ""

""  \| a = a
a   \| _ = a

0 |\ a = a
a |\ _ = a

-- |Logical OR for 'Artifact'. If both fields are not empty, resulting field
-- will have value from the first argument.
a <|||> b = Artifact
    (a_id a     |\ a_id b)
    (a_path a   \| a_path b)
    (a_status a \| a_status b)
    (a_time a   \| a_time b)
    (a_alias a  \| a_alias b)
    (a_jobid a  |\ a_jobid b)
    (a_type a   \| a_type b)
    (a_uri a    \| a_uri b)

-- |Logical OR for 'ArtifactFilter'. Use it like
--
-- @
--   'filterID' 5 \<||\> 'filterName' "name" \<||\>
--   ('ArtifactFilter' $ 'emptyArtifact'{'a_type' = \"READY\"})
-- @
a <||> b = ArtifactFilter (a_filter a <|||> a_filter b)
