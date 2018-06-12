{-# LANGUAGE DeriveGeneric #-}

module Festral.Tests.Config (
    TestConfig (..),
    TestRunnerConfig (..)
) where

import Data.Aeson
import GHC.Generics

data TestConfig = TestConfig
    { repo  :: String
    , yaml  :: FilePath
    , parser:: String
    } deriving (Show, Generic)

instance FromJSON TestConfig
instance ToJSON TestConfig

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
