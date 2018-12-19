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

-- |Library which implements API of the Boruta - manager of the device farm in
-- the SLAV stack. It allows to perform low level actions on the devices in the
-- farm, push files, executing commands, booting up devices under test (DUT),
-- and get informations about workers and jobs.
module Festral.SLAV.Boruta (
    curlWorkers,
    allRequests,
    execAnyDryadConsole,
    execSpecifiedDryadConsole,
    closeRequest,
    execMuxPi,
    execDUT,
    pushMuxPi,
    pushDUT,
    dutBoot,
    createRequest,
    workerDeviceType,
    setMaintenace,
    setIdle,
    setState
) where

import Control.Applicative
import Control.Exception
import Network.Curl
import Network.Curl.Aeson
import Data.Time.Clock
import Data.Time.Format
import System.Process
import System.IO
import System.IO.Temp
import Data.List.Split
import Control.Exception
import Control.Concurrent
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import Data.Aeson
import qualified Control.Monad.Parallel as Par
import System.FilePath.Posix

import Festral.Internal.Files
import Festral.Internal.Logger
import Festral.Config
import Festral.SLAV.Boruta.Data

-- |Return device type of the given worker
workerDeviceType :: Worker -> String
workerDeviceType worker = device_type $ caps worker

borutaAddr = do
    config <- getAppConfig
    return $ (borutaIP config, borutaPort config)

-- |Returns list of workers of boruta under address declared in the application
-- configuration file "Festral.Config".
curlWorkers :: IO [Worker]
curlWorkers = do
    (addr, port) <- borutaAddr
    (err, str) <-
        curlGetString
            (addr ++ ":" ++ show port ++ "/api/workers")
            [CurlFollowLocation True]
    return $ fromMaybe [] (decode (BL.pack str) :: Maybe [Worker])

-- |Create request for given target defined by `Caps` with given priority and
-- time to live. Returns ID of the created request or 'Nothing'.
createRequest :: Caps            -- ^ Device information of required device
              -> Int             -- ^ Priority of the request
              -> NominalDiffTime -- ^ Time to live of the request in the seconds
              -> IO (Maybe Int)
createRequest caps priority timeout = do
    time <- getCurrentTime
    let now = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time
    let afterHour = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $
                    addUTCTime timeout time
    let request = BorutaRequestCreate afterHour now caps priority
    (addr, port) <- borutaAddr
    req <- handle handleCurlInt $ Just <$> ((curlAeson
            parseJSON
            "POST"
            (addr ++ ":" ++ show port ++ "/api/reqs/")
            [CurlFollowLocation True]
            (Just request)) :: IO ReqID)
    return $ simpleReqID <$> req
    where
        handleCurlInt :: CurlAesonException -> IO (Maybe ReqID)
        handleCurlInt e = putStrColor Yellow "Can't create new request\n"
                            >> return Nothing

-- |List all requests from Boruta
allRequests :: IO [BorutaRequest]
allRequests = do
    (addr, port) <- borutaAddr
    (err, str) <-
        curlGetString
            (addr ++ ":" ++ show port ++ "/api/reqs")
            [CurlFollowLocation True]
    return $ fromMaybe [] (decode (BL.pack str) :: Maybe [BorutaRequest])

-- |Get request information specified by its Id.
getRequestById :: Int -> IO (Maybe BorutaRequest)
getRequestById id = listToMaybe <$>
    filter (\ x -> reqID x == id) <$> allRequests

activeStatuses = ["IN PROGRESS", "WAITING"]

-- |Return ssh key for session for given by name target and request ID.
-- If this target has running session it returns key for it,
-- othervise it opens new request
getTargetAuth :: (BorutaRequest -> Bool) -> Caps -> IO (Maybe BorutaAuth)
getTargetAuth selector caps = do
    requests <- allRequests
    let active = filter (\ x -> (selector x)
                        && ((reqState x) `elem` activeStatuses))
                requests
    id <- if length active == 0
        then createRequest caps 4 3600
        else do
            putStrColor Yellow $ "Target is busy by request with ID "
                ++ show (reqID $ head active) ++ "\n"
            return Nothing
    maybe (return Nothing) getKey id

-- |The same as "getTargetAuth" but connect busy session instead of reject
-- request such request.
getBusyTargetAuth :: (BorutaRequest -> Bool) -> Caps
    -> IO (Maybe BorutaAuth)
getBusyTargetAuth selector caps = do
    requests <- allRequests
    let active = filter (\ x ->
                (selector x) && (reqState x) == "IN PROGRESS") requests
    id <- if length active == 0
        then createRequest caps 4 3600
        else return $ Just $ reqID $ head active
    maybe (return Nothing) getKey id


getSpecifiedTargetAuth targetUUID f = do
    let caps = Caps "" "" "" targetUUID
    let selector = (\ x -> (uuid $ reqCapsIn x) == targetUUID)
    f selector caps

-- |Get any accessible device of given device_type
getDeviceTypeAuth device f = do
    let caps = Caps ""  "" device ""
    let selector = (\ x -> (device_type (reqCapsIn x) == device)
                    || deviceType (reqCapsIn x) == device)
    f selector caps

getKey :: Int -> IO (Maybe BorutaAuth)
getKey id = do
    (addr, port) <- borutaAddr
    auth <- handle (curlHandler id) $ Just <$> curlAeson
        parseJSON
        "POST"
        (addr ++ ":" ++ show port ++ "/api/v1/reqs/"
            ++ show id ++ "/acquire_worker")
        [CurlFollowLocation True]
        (Nothing :: Maybe Caps)
    maybe (return Nothing) (\ x -> return $ Just (x{authReqId=id})) auth

curlHandler :: Int -> CurlAesonException -> IO (Maybe BorutaAuth)
curlHandler id e = do
    threadDelay oneSec
    getKey id

authMethod True = getBusyTargetAuth
authMethod _ = getTargetAuth

-- |Close authorised request. Wait until it will defenitely be closed.
closeAuth auth = do
    maybe (return ()) closeAuth' auth

closeAuth' auth = do
    closeRequest $ authReqId auth
    waitJobClosed auth

waitJobClosed auth = do
    req <- getRequestById $ authReqId auth
    maybe (return ()) (\ req ->
        if any (\ x -> reqState req == x) activeStatuses
            then threadDelay oneSec >> waitJobClosed auth
            else return ()) req

-- |Run ssh session for any device which matches specified 'device_type'.
-- Second parameter decide if connect forcely if device is busy.
execAnyDryadConsole :: String   -- ^ Device type
                    -> Bool     -- ^ Enforce connection
                    -> IO ()
execAnyDryadConsole x f = do
    auth <- getDeviceTypeAuth x (authMethod f)
    execDryad (sshCmd "") auth
    closeAuth auth

-- |Run ssh session for device specified by its UUID
execSpecifiedDryadConsole :: String -- ^ Device UUID
                          -> Bool   -- ^ Enforce connection
                          -> IO ()
execSpecifiedDryadConsole x f = do
    auth <- getSpecifiedTargetAuth x (authMethod f)
    execDryad (sshCmd "") auth
    closeAuth auth

-- |Execute command on MuxPi
execMuxPi :: String -- ^ UUID of the device
          -> String -- ^ Command
          -> Bool   -- ^ Enforce connection
          -> IO ()
execMuxPi uid cmd f = do
    auth <- getSpecifiedTargetAuth uid (authMethod f)
    execDryad (sshCmd cmd) auth
    closeAuth auth

-- |Execute command on the device under test of the Dryad specified by UUID
execDUT :: String   -- ^ UUID of the device
        -> String -- ^ Command
        -> Bool     -- ^ Enforce connection
        -> IO ()
execDUT uid cmd f = do
    auth <-  getSpecifiedTargetAuth uid (authMethod f)
    execDryad (sshCmd ./"dut_exec.sh " ++ cmd) auth
    closeAuth auth

-- |Push file from host to the MuxPi of Dryad identified by UUID
pushMuxPi :: String     -- ^ UUID of the device
          -> [FilePath] -- ^ Files to be copied from host
          -> FilePath   -- ^ Destination file name on the MuxPi
          -> Bool       -- ^ Enforce connection
          -> IO ()
pushMuxPi uid sources to f = do
    auth <- getSpecifiedTargetAuth uid (authMethod f)
    Par.mapM_ (\ from -> execDryad (scpCmd from to) auth) sources
    closeAuth auth

-- |The same as 'pushMuxPi' but push file to the device under test
pushDUT :: String   -- ^ UUID of the device
        -> [FilePath]-- ^ Files to be copied from host
        -> FilePath -- ^ Destination file name on the Device Under Test (DUT)
        -> Bool     -- ^ Enforce connection
        -> IO ()
pushDUT uid sources to f = do
    auth <- getSpecifiedTargetAuth uid (authMethod f)
    Par.mapM_ (\ from -> do
        execDryad (scpCmd from $ tmpfile from) auth
        execDryad (sshCmd ./ "dut_copyto.sh " ++ tmpfile from ++ " " ++ to)
            auth
        execDryad (sshCmd $ "rm " ++ tmpfile from) auth) sources
    closeAuth auth
    where
        tmpfile x = "/tmp/" ++ takeFileName x

sshCmd cmd x = "ssh -oStrictHostKeyChecking=no "
    ++ dsUser x ++ "@" ++ dsIp x ++ " -p " ++ show (dsPort x)
    ++ " -i " ++ idFile x ++ " " ++ cmd
scpCmd fname out x = "scp -oStrictHostKeyChecking=no "
    ++ "-i " ++ idFile x ++ " -P " ++ show (dsPort x) ++ " "
    ++ fname ++ " " ++ dsUser x
    ++ "@" ++ dsIp x ++ ":" ++ out

type DryadCmd = (DryadSSH -> String)

-- |This function takes function converting dryad connection data to the command
-- and this function's argument and executes this command.
execDryad :: DryadCmd -> Maybe BorutaAuth -> IO ()
execDryad _ Nothing = do
    putStrLn "Use --force option to connect for existing session if you are \
    \sure you know you do."
    putColorBold Red "IMPORTANT: "
    putStr "force connecting to the running job will cause \
    \closing it after your command done, so "
    putColorBold Red "YOU MAY BROKE OTHER'S WORK!!!\n"
execDryad f (Just auth) = do
    (addr, _) <- borutaAddr
    keyFile <- writeKey auth
    let creds = DryadSSH
                (username auth) addr (port $ authAddr auth) keyFile
    handle h $ callCommand $ f creds
    where
        h :: SomeException -> IO ()
        h _ = return ()

writeKey auth = writeSystemTempFile "boruta-key" (sshKey auth)

-- |Close Boruta request specified by its ID
closeRequest :: Int -> IO ()
closeRequest id = do
    (addr, port) <- borutaAddr
    handle h $ curlAeson
        parseJSON
        "POST"
        (addr ++ ":" ++ show port ++ "/api/v1/reqs/" ++ show id ++ "/close")
        [CurlFollowLocation True]
        (Nothing :: Maybe Caps)
    where
        h :: CurlAesonException -> IO ()
        h _ = putStrColor Yellow "Cant access requesteed ID\n" >> return ()

-- |Boot up Device Under Test specified by UUID
dutBoot uid = do
    auth <- getSpecifiedTargetAuth uid (authMethod False)
    execDryad (sshCmd ./"dut_boot.sh") auth
    execDryad (sshCmd ./"dut_login.sh root") auth
    closeAuth auth

-- |Set specified by UUID dryad in MAINTENANCE mode: Weles will not be able to
-- start tests on it.
setMaintenace :: String -> IO ()
setMaintenace uuid = setState (WorkerState "MAINTENANCE") uuid

-- |Set specified by UUID dryad in IDLE mode: it can be used by Weles normally.
setIdle :: String -> IO ()
setIdle uuid = setState (WorkerState "IDLE") uuid

-- |Set specified by UUID dryad in specified 'WorkerState' mode.
setState :: WorkerState -> String -> IO ()
setState req uuid = do
    (addr, port) <- borutaAddr
    (curlAeson
            parseJSON
            "POST"
            (addr ++ ":" ++ show port ++
                "/api/v1/workers/" ++ uuid ++ "/setstate")
            [CurlFollowLocation True]
            (Just req)) :: IO ()

-- |Prepend executable path to the string and execute function
infixr 4 ./
(./) :: (String -> a) -> String -> a
f ./ x = f $ "/usr/local/bin/" ++ x

oneSec = 1000000
