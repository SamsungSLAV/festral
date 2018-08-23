module Festral.Files (
    freshBuilds,
    freshTests,
    buildCache,
    configFile,
    getAppConfig
) where

import System.Directory
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Festral.Tests.Config
import Control.Monad

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
    exists <- doesFileExist confPath
    when (not exists) $ LB.writeFile confPath $ encode (TestRunnerConfig "/tmp/build" "/tmp/test" "127.0.0.1" "6666" "6666" "127.0.0.1")
    confStr <- LB.readFile confPath
    let Just config = decode confStr :: Maybe TestRunnerConfig
    return config
