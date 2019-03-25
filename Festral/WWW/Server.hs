{-
 - Copyright (c) 2018-2019 Samsung Electronics Co., Ltd All Rights Reserved
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

{-# LANGUAGE OverloadedStrings #-}

module Festral.WWW.Server (
    runServerOnPort,
    runFileServerOnly
)
where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid
import Data.List.Split
import Data.List
import qualified Data.ByteString.UTF8   as BSU
import qualified Data.ByteString.Char8  as BS
import System.IO
import System.Directory
import Control.Exception
import Network.HTTP.Types.Header
import System.FilePath
import qualified Data.Text as T

import Festral.WWW.TestGUI
import Festral.Internal.Files
import Festral.Config
import Festral.Meta

data ContentType
    = Download
    | Directory

-- |Run server on port given in configuration.
runServerOnPort config = do
    run (webPagePort config) $ fileServer config

-- |Run server on port given in configuration.
runFileServerOnly config = do
    run (webPagePort config) $ fileServerOnly config

fileServerOnly config req respond = do
    respondFileServerOnly config respond $ T.unpack <$> pathInfo req

fileServer config req respond = do
    let opts = parseQuery $ show $ rawQueryString req
    sendRespond opts config respond $ T.unpack <$> pathInfo req

sendRespond opts config r ["download"    ] = r $ download opts config
sendRespond opts config r ["download.php"] = r $ download opts config
sendRespond opts config r ["getlog.php"  ] = sendRespond opts config
    r ["getlog"]
sendRespond opts config r ["getlog"      ] = do
    log <- readLog opts config
    r $ getlog log
sendRespond opts config r ("reports"    :xs) = sendRespond opts config
    r $ ["files"] ++ xs
sendRespond opts config r ("reports.php":xs) = sendRespond opts config
    r $ ["files"] ++ xs
sendRespond opts config r ("files"      :xs) =
    listFiles config (removeRelPaths xs) "/files/" >>= r
sendRespond opts config r ("secosci":x) = sendRespond opts config r x
sendRespond a b c d = indexRespond a b c d

removeRelPaths = intercalate "/"
    . filter (\ x -> not $ any (x ==) ["", ".", ".."])

respondFileServerOnly config r [] = listFiles config "" "" >>= r
respondFileServerOnly config r path =
    listFiles config (removeRelPaths path) "/" >>= r

download opts config = do
    let dir = buildLogDir config
    let build_hash = findField "build" opts
    let fname = findField "file" opts
    responseFile status200 (contentTypeFromExt "" fname)
        (dir ++ "/" ++ build_hash ++ "/" ++ fname) Nothing

listFiles :: AppConfig -> String -> String -> IO Response
listFiles config fullpath prefix = do
    isDirectory <- doesDirectoryExist $ (serverRoot config) ++ "/" ++ fullpath
    let ftype = if isDirectory
                    then Directory
                    else Download
    showFile ftype config fullpath prefix

showFile :: ContentType -> AppConfig -> FilePath -> String -> IO Response
showFile Directory config fullpath prefix = do
    reports <- listDirectory $ (serverRoot config) ++ "/" ++ fullpath
    return $ responseBuilder status200 [("Content-Type", "text/html")]
        $ mconcat $ map copyByteString
        $ map (\x -> BSU.fromString $ "<a href=\""
            ++ prefix ++ fullpath ++ "/" ++ x ++"\">"
        ++ x ++"</a><br>") (sort reports)
showFile Download config x _ = do
            return $ responseFile status200
                (contentTypeFromExt (takeExtension x) x)
                (serverRoot config ++ "/" ++ x) Nothing

contentTypeFromExt ".html" _ = [(hContentType, "text/html")]
contentTypeFromExt ".htm" _ = contentTypeFromExt ".html" ""
contentTypeFromExt ".css" _ = [(hContentType, "text/css")]
contentTypeFromExt _ fname =
        [
            (hContentDisposition, BSU.fromString
                $ "attachment; filename=\""++ takeFileName fname ++"\""),
            (hContentType, "application/octet-stream")
        ]

readLog opts config = do
    let logtype = findField "type" opts
    let (dir, fname) = resolveDir logtype
    let hash = findField "hash" opts
    let time = findField "time" opts
    let fpath = dir ++ "/" ++ hash ++ "_" ++
                (if logtype == "build"
                    then "B"
                    else "T")
                ++ time ++ "/" ++ fname
    BS.unpack <$> BS.readFile fpath
    where
        -- return pair (directory to the log/build from config, logfile name
        -- dependent on type of log)
        resolveDir "build" = (buildLogDir config, "build.log")
        resolveDir "test" = (testLogDir config, "tf.log")
        resolveDir "" = ("","")

getlog log = responseBuilder status200
        [("Content-Type", "text/html")] $
        mconcat $ map copyByteString ["<pre>", BSU.fromString log, "</pre>"]

parseQuery :: String -> [[String]]
parseQuery x = map (splitOn "=") $ filter (not . null) $ splitOneOf "?&\"" x
