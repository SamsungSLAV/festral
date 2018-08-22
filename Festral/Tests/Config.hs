{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- |This module describes Festral's configuration file structure and serialization methods.
-- Configuration file is just JSON file with fields as in 'TestRunnerConfig'.
module Festral.Tests.Config (
    TestConfig (..),
    TestRunnerConfig (..)
) where

import Data.Aeson
import GHC.Generics
import Control.Applicative

-- |This data structure describes test configuration for one repository specified by name.
data TestConfig = TestConfig
    { repo  :: String
    , yaml  :: FilePath
    , parser:: String
    , name :: String
    } deriving (Show, Generic)

instance FromJSON TestConfig where
    parseJSON = withObject "TestConfig" $ \o -> do
        repo <- o .: "repo"
        yaml <- o .: "yaml"
        parser <- o .: "parser"
        name <- o .:? "name" .!= "unknown"
        return TestConfig{..}

instance ToJSON TestConfig

-- |Describes basic festral-weles application configuration.
data TestRunnerConfig = TestRunnerConfig
    { buildLogDir   :: FilePath
    , testLogDir    :: FilePath
    , welesIP       :: String
    , welesPort     :: String
    , welesFilePort :: String
    , webPageIP     :: String
    , yamls         :: [TestConfig]
    } deriving (Show, Generic)

instance FromJSON TestRunnerConfig
instance ToJSON TestRunnerConfig
