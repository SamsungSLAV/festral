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

{-# LANGUAGE InstanceSigs #-}

-- |GBSParser module implements 'MetaParser' class and parse output of usual
-- GBS build to the 'Meta'.
module Festral.Builder.GBSParser (
    GBSParser (..),
    parse,
    fromFile
) where

import Festral.Meta
import Data.List.Split
import Data.List
import Control.Exception
import Data.Char
import Data.Time
import System.IO
import Festral.Files

-- |This data represents parser and contains files needed for parser.
data GBSParser = GBSParser
    { buildHtml :: String -- ^Html file with some additional build outputs. Path to the file is automatically extracted from 'buildLog'.
    , buildLog  :: String -- ^File with stdout of gbs build command.
    } deriving Show

instance MetaParser GBSParser where

    parse :: GBSParser -> IO (Maybe Meta)
    parse gbsParser = do
        let content = buildHtml gbsParser
        time <- show <$> getZonedTime
        let (year:mounth:day:hour:min:secs:_) = splitOneOf " :-." time
        let time = year ++ mounth ++ day ++ hour ++ min ++ secs

        let stat = map (takeWhile ((/= '<'))) $ map (!!1) $ map
                    (splitOn "<td>") $ take 2 $
                    filter (isInfixOf "<td>") $ lines content
        let [total, succ] = parseStat $ (map read stat) :: [Int]
        let status = if total > succ || total <= 0
                     then "FAILED"
                     else "SUCCEED"

        let log = buildLog gbsParser
        let arch = dropWhile isSpace $ last $ splitOn "</B>" $ head $
                    splitOn "</p>" $ last $  splitOn "Arch" content
        let arch' = if arch == "" then "unknown" else arch
        let out = dropWhile isSpace $ head $ splitOn "\n" $ last $
                    splitOn "generated RPM packages can be found from \
                    \local repo:\n" log
        return $ Just $ Meta (MetaBase
                        arch'
                        "unknown"
                        "unknown"
                        time
                        "GBS"
                        "unknown"
                        status
                        (replicate 40 '0')
                        out
                        "unknown"
                        "unknown")
        where
            parseStat x@[_,_] = x
            parseStat _ = [0, -1]

    fromFile :: FilePath -> IO GBSParser
    fromFile flog = fromFile' flog

    fromHandle :: Handle -> IO GBSParser
    fromHandle flog = do
        log <- hGetContents flog
        let fhtml = last $ splitOneOf " " $ head $ lines $ last $
                    splitOn "generated html format report:\n" log
        html <- safeReadFile fhtml
        return $ GBSParser html log

fromFile' :: FilePath -> IO GBSParser
fromFile' flog = do
    log <- safeReadFile flog
    let fhtml = last $ splitOneOf " " $ head $ lines $ last $
                splitOn "generated html format report:\n" log
    html <- safeReadFile fhtml
    return $ GBSParser html log
