{-# LANGUAGE DeriveGeneric #-}

-- |This module describes Festral's configuration file structure and serialization methods.
-- Configuration file is just JSON file with fields as in 'TestRunnerConfig'.
module Festral.Tests.Config (
    TestConfig (..),
    TestRunnerConfig (..)
) where

import Data.Aeson
import GHC.Generics

-- |This data structure describes test configuration for one repository specified by name.
data TestConfig = TestConfig
    { repo  :: String
    , yaml  :: FilePath
    , parser:: String
    } deriving (Show, Generic)

instance FromJSON TestConfig
instance ToJSON TestConfig

-- |Describes basic festral-weles application configuration.
data TestRunnerConfig = TestRunnerConfig
    { buildLogDir   :: FilePath
    , testLogDir    :: FilePath
    , welesIP       :: String
    , welesPort     :: String
    , welesFilePort :: String
    , yamls         :: [TestConfig]
    } deriving (Show, Generic)

instance FromJSON TestRunnerConfig
instance ToJSON TestRunnerConfig
