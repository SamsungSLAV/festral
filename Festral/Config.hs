{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |This module describes Festral's configuration file structure and serialization methods.
-- Configuration file is just JSON file with fields as in 'TestRunnerConfig'.
module Festral.Config (
    TestConfig (..),
    AppConfig (..)
) where

import Data.Aeson
import GHC.Generics
import Control.Applicative

-- |This data structure describes test configuration for one repository specified by name.
data TestConfig = TestConfig
    { repo  :: String
    , yaml  :: FilePath
    , parser:: String
    , name  :: String
    } deriving (Show, Generic)

instance FromJSON TestConfig where
    parseJSON = withObject "TestConfig" $ \o -> do
        repo <- o .: "repo"
        yaml <- o .: "yaml"
        parser <- o .: "parser"
        name <- o .:? "name" .!= "unknown"
        return TestConfig{..}

instance ToJSON TestConfig

-- |Describes basic festral application configuration.
data AppConfig = AppConfig
    { buildLogDir   :: FilePath
    , testLogDir    :: FilePath
    , welesIP       :: String
    , welesPort     :: String
    , welesFilePort :: String
    , webPageIP     :: String
    , serverRoot    :: FilePath
    , borutaIP      :: String
    } deriving (Show, Generic)

instance FromJSON AppConfig where
    parseJSON = withObject "AppConfig" $ \o -> do
        buildLogDir     <- o .:? "buildLogDir"  .!= "/tmp/builds"
        testLogDir      <- o .:? "testLogDir"   .!= "/tmp/tests"
        welesIP         <- o .:? "welesIP"      .!= "127.0.0.1"
        welesPort       <- o .:? "welesPort"    .!= "8888"
        welesFilePort   <- o .:? "welesFilePort".!= "8888"
        webPageIP       <- o .:? "webPageIP"    .!= "127.0.0.1:8888"
        serverRoot      <- o .:? "serverRoot"   .!= "/tmp/festral_server"
        borutaIP        <- o .:? "borutaIP"     .!= "127.0.0.1:6666"
        return AppConfig{..}
instance ToJSON AppConfig
