#!/usr/bin/runhaskell

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
