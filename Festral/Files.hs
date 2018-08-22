module Festral.Files (
    freshBuilds,
    freshTests,
    buildCache,
    configFile,
    progVersion,
    getAppConfig
) where

import System.Directory
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Festral.Tests.Config

progVersion = "0.6.0 Alpha"

freshBuilds = do
    x <- getHomeDirectory
    createDirectoryIfMissing False $ x ++ "/.festral"
    return $ x ++ "/.festral/fresh_builds"

buildCache = do
    home <- getHomeDirectory
    createDirectoryIfMissing False $ home ++ "/.festral"
    return $ home ++ "/.festral/build.cachce"

freshTests = do
    x <- getHomeDirectory
    createDirectoryIfMissing False $ x ++ "/.festral"
    return $ x ++ "/.festral/fresh_tests"

configFile = do
    home <- getHomeDirectory
    return $ home ++ "/.festral.conf"

getAppConfig = do
    confPath <- configFile
    confStr <- LB.readFile confPath
    let Just config = decode confStr :: Maybe TestRunnerConfig
    return config
