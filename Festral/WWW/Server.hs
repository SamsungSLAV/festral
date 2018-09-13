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
 
runServerOnPort port = do
    putStrLn $ "Listening on port " ++ show port
    run port fileServer
 
fileServer req respond = do
    config <- getAppConfig
    let opts = parseQuery $ show $ rawQueryString req
    sendRespond opts config respond $ pathInfo req

sendRespond opts config r ["secosci", "download"] = r $ download opts config
sendRespond opts config r ["secosci", "download.php"] = r $ download opts config
sendRespond opts config r ["secosci", "getlog.php"] = sendRespond opts config r ["secosci", "getlog"]
sendRespond opts config r ["secosci", "getlog"] = do
    log <- readLog opts config
    r $ getlog log
sendRespond opts config r ["secosci", "reports.php"] = sendRespond opts config r ["secosci", "reports"]
sendRespond opts config r ["secosci", "reports"] = do
    reports <- listDirectory $ reportsDir config
    r $ listReports reports config opts
sendRespond a b c d = indexRespond a b c d

download opts config = do
    let dir = buildLogDir config
    let build_hash = findField "build" opts
    let fname = findField "file" opts
    responseFile status200 
        [ 
            ("Content-Disposition", BSU.fromString $ "attachment; filename=\""++ fname ++"\""), 
            ("Content-Type", "application/octet-stream") 
        ] 
        (dir ++ "/" ++ build_hash ++ "/" ++ fname) Nothing

listReports reports config opts = do
    let fname = findField "file" opts
    report fname

    where
        report "" = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
            $ map (\x -> BSU.fromString $ "<a href=\"reports?file="++ x ++"\">"++ x ++"</a><br>") (sort reports)
        report x = responseFile status200 [("Content-Type", "text/html")] (reportsDir config ++ "/" ++ x) Nothing

readLog opts config = do
    let logtype = findField "type" opts
    let (dir, fname) = resolveDir logtype
    let hash = findField "hash" opts
    let time = findField "time" opts
    let fpath = dir ++ "/" ++ hash ++ "_" ++ time ++ "/" ++ fname
    BS.unpack <$> BS.readFile fpath
    where
        -- return pair (directory to the log/build from config, logfile name dependent on type of log)
        resolveDir "build" = (buildLogDir config, "build.log")
        resolveDir "test" = (testLogDir config, "tf.log")
        resolveDir "" = ("","")
    
getlog log = responseBuilder status200
        [("Content-Type", "text/html")] $
        mconcat $ map copyByteString ["<pre>", BSU.fromString log, "</pre>"]
 
parseQuery :: String -> [[String]]
parseQuery x = map (splitOn "=") $ filter (not . null) $ splitOneOf "?&\"" x
