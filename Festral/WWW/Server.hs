{-# LANGUAGE OverloadedStrings #-}

module Festral.WWW.Server (
    runServerOnPort
)
where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404)
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid
import Festral.Builder.Meta
import Festral.Files
import Festral.Config
import Data.List.Split
import Data.List
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Char8 as BS
import System.IO
import System.Directory
import Festral.WWW.TestGUI
import Control.Exception
import Network.HTTP.Types.Header
import System.FilePath

runServerOnPort port = do
    putStrLn $ "Listening on port " ++ show port
    run port fileServer

fileServer req respond = do
    config <- getAppConfig
    let opts = parseQuery $ show $ rawQueryString req
    sendRespond opts config respond $ pathInfo req

sendRespond opts config r ["secosci", "download"    ] = r $ download opts config
sendRespond opts config r ["secosci", "download.php"] = r $ download opts config
sendRespond opts config r ["secosci", "getlog.php"  ] = sendRespond opts config
    r ["secosci", "getlog"]
sendRespond opts config r ["secosci", "getlog"      ] = do
    log <- readLog opts config
    r $ getlog log
sendRespond opts config r ["secosci", "files"       ] = sendRespond opts config
    r ["secosci", "reports"]
sendRespond opts config r ["secosci", "reports.php" ] = sendRespond opts config
    r ["secosci", "reports"]
sendRespond opts config r ["secosci", "reports"     ] = do
    reports <- listDirectory $ serverRoot config
    r $ listFiles reports config opts
sendRespond a b c d = indexRespond a b c d

download opts config = do
    let dir = buildLogDir config
    let build_hash = findField "build" opts
    let fname = findField "file" opts
    responseFile status200 (contentTypeFromExt "" fname)
        (dir ++ "/" ++ build_hash ++ "/" ++ fname) Nothing

listFiles reports config opts = do
    let fname = findField "file" opts
    showFile fname

    where
        showFile "" = responseBuilder status200 [("Content-Type", "text/html")]
            $ mconcat $ map copyByteString
            $ map (\x -> BSU.fromString $ "<a href=\"files?file="++ x ++"\">"
            ++ x ++"</a><br>") (sort reports)
        showFile x = responseFile status200
                    (contentTypeFromExt (takeExtension x) x)
                    (serverRoot config ++ "/" ++ x) Nothing

contentTypeFromExt ".html" _ = [(hContentType, "text/html")]
contentTypeFromExt ".htm" _ = contentTypeFromExt ".html" ""
contentTypeFromExt ".css" _ = [(hContentType, "text/css")]
contentTypeFromExt _ fname =
        [
            (hContentDisposition, BSU.fromString
                $ "attachment; filename=\""++ fname ++"\""),
            (hContentType, "application/octet-stream")
        ]

readLog opts config = do
    let logtype = findField "type" opts
    let (dir, fname) = resolveDir logtype
    let hash = findField "hash" opts
    let time = findField "time" opts
    let fpath = dir ++ "/" ++ hash ++ "_" ++ time ++ "/" ++ fname
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
