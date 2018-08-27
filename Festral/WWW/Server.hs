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
import System.Directory
 
runServerOnPort port = do
    putStrLn $ "Listening on port " ++ show port
    run port fileServer
 
fileServer req respond = do
    config <- getAppConfig
    let opts = parseQuery $ show $ rawQueryString req
    reports <- listDirectory $ reportsDir config
    respond $ case pathInfo req of
        ["secosci", "download"]     -> download opts config
        ["secosci", "download.php"] -> download opts config
        ["secosci", "getlog"]       -> getlog opts config
        ["secosci", "getlog.php"]   -> getlog opts config
        ["secosci", "reports"]      -> listReports reports config opts
        ["secosci", "reports.php"]  -> listReports reports config opts
        x -> index x
 
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
            $ map (\x -> BSU.fromString $ "<a href=\"reports?file="++ reportsDir config ++ "/" ++ x ++"\">"++ x ++"</a><br>") (sort reports)
        report x = responseFile status200 [("Content-Type", "text/html")] x Nothing

getlog opts config = do
    let logtype = findField "type" opts
    let (dir, fname) = resolveDir logtype
    let hash = findField "hash" opts
    let time = findField "time" opts
    responseFile status200
        [("Content-Type", "text/html")]
        (dir ++ "/" ++ hash ++ "_" ++ time ++ "/" ++ fname)
        Nothing

    where
        -- return pair (directory to the log/build from config, logfile name dependent on type of log)
        resolveDir "build" = (buildLogDir config, "build.log")
        resolveDir "test" = (testLogDir config, "tf.log")
        resolveDir "" = ("","")
 
index x = responseBuilder status404 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>This page does not exists ", BSU.fromString $ show x, "!</p>"]

parseQuery :: String -> [[String]]
parseQuery x = map (splitOn "=") $ filter (not . null) $ splitOneOf "?&\"" x
