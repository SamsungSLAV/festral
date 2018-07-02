module Festral.Files (
    freshBuilds,
    freshTests,
    buildCache,
    configFile
) where

import System.Directory

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
