#!/usr/bin/runhaskell

import System.Environment
import Data.Time
import Data.List.Split
import Data.List

main=do
    args <- getArgs

    time <- show <$> getZonedTime
    let (year:mounth:day:hour:min:secs:_) = splitOneOf " :-." time
    let time = year ++ mounth ++ day ++ hour ++ min ++ secs

    let status = if "error" `isInfixOf` head args
                    then "FAILED"
                    else "SUCCEED"

    putStrLn $ "BOARD=x86_64"
    putStrLn $ "BUILD_TYPE=debug"
    putStrLn $ "COMMIT=unknown"
    putStrLn $ "BUILD_TIME=" ++ time
    putStrLn $ "TOOLCHAIN=ghc"
    putStrLn $ "BUILDER=unknown"
    putStrLn $ "BUILD_STATUS=" ++ status
    putStrLn $ "BUILD_HASH=unknown"
    putStrLn $ "OUT_DIR=/tmp/festral/dist/build/"
    putStrLn $ "REPO_NAME=unknown"
    putStrLn $ "BRANCH=unknown"
