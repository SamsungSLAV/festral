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
--
-- In this module, all 'NetAddress' values passed to the functions must contains
-- only ip address and REST API port of Boruta will be requested. For make
-- 'NetAddress' from these parameters use 'simpleAddress' function.
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
    setState,
    RequestOptions(..)
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
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import Data.Aeson
import qualified Control.Monad.Parallel as Par
import System.FilePath.Posix
import Data.UUID

import Festral.Internal.Logger
import Festral.Config
import Festral.SLAV.Boruta.Data

-- |Return device type of the given worker
workerDeviceType :: Worker -> String
workerDeviceType worker = device_type $ caps worker

-- |Returns list of workers of Boruta under given network address. Use
-- 'simpleAddress' function for make addres for it.
curlWorkers :: NetAddress -> IO [Worker]
curlWorkers borutaAddr = do
    let (addr, port) = getAddr borutaAddr
    (err, str) <-
        curlGetString
            (addr ++ ":" ++ show port ++ "/api/workers")
            [CurlFollowLocation True]
    return $ fromMaybe [] (decode (BL.pack str) :: Maybe [Worker])

-- |Create request for given target defined by `Caps` with given priority and
-- time to live. Returns ID of the created request or 'Nothing'.
createRequest :: NetAddress      -- ^ Network address of Boruta API server
              -> Caps            -- ^ Device information of required device
              -> Int             -- ^ Priority of the request
              -> NominalDiffTime -- ^ Time to live of the request in the seconds
              -> IO (Maybe Int)
createRequest borutaAddr caps priority timeout = do
    time <- getCurrentTime
    let now = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time
    let afterHour = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $
                    addUTCTime timeout time
    let request = BorutaRequestCreate afterHour now caps priority
    let (addr, port) = getAddr borutaAddr
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

-- |List all requests from Boruta listening under given address.
-- Use 'simpleAddress' for make it.
allRequests :: NetAddress -> IO [BorutaRequest]
allRequests borutaAddr = do
    let (addr, port) = getAddr borutaAddr
    (err, str) <-
        curlGetString
            (addr ++ ":" ++ show port ++ "/api/reqs")
            [CurlFollowLocation True]
    return $ fromMaybe [] (decode (BL.pack str) :: Maybe [BorutaRequest])

-- |Get request information specified by its Id.
-- See also 'allRequests' for more details.
getRequestById :: NetAddress -> Int -> IO (Maybe BorutaRequest)
getRequestById addr id = listToMaybe <$>
    filter (\ x -> reqID x == id) <$> allRequests addr

activeStatuses = ["IN PROGRESS", "WAITING"]

-- |Return ssh key for session for given by name target and request ID.
-- If this target has running session it returns key for it,
-- othervise it opens new request.
getTargetAuth :: NetAddress             -- ^ Address of the Boruta API made by 'simpleAddress' function
              -> (BorutaRequest -> Bool)-- ^ BorutaRequest recieved from Boruta
              -> Caps                   -- ^ Capabilities of request
              -> IO (Maybe BorutaAuth)
getTargetAuth addr selector caps = do
    requests <- allRequests addr
    let active = filter (\ x -> (selector x)
                        && ((reqState x) `elem` activeStatuses))
                requests
    id <- if length active == 0
        then createRequest addr caps 4 3600
        else do
            putStrColor Yellow $ "Target is busy by request with ID "
                ++ show (reqID $ head active) ++ "\n"
            return Nothing
    maybe (return Nothing) (getKey addr) id

-- |The same as "getTargetAuth" but connect busy session instead of reject
-- request such request.
getBusyTargetAuth :: NetAddress             -- ^ Address of the Boruta API made by 'simpleAddress' function
                  -> (BorutaRequest -> Bool)-- ^ BorutaRequest from Boruta
                  -> Caps                   -- ^ Capabilities of request
                  -> IO (Maybe BorutaAuth)
getBusyTargetAuth addr selector caps = do
    requests <- allRequests addr
    let active = filter (\ x ->
                (selector x) && (reqState x) == "IN PROGRESS") requests
    id <- if length active == 0
        then createRequest addr caps 4 3600
        else return $ Just $ reqID $ head active
    maybe (return Nothing) (getKey addr) id


getSpecifiedTargetAuth id f = do
    let targetUUID = toString id
    let caps = Caps "" "" targetUUID
    let selector = (\ x -> (uuid $ reqCapsIn x) == targetUUID)
    f selector caps

-- |Get any accessible device of given device_type
getDeviceTypeAuth device f = do
    let caps = Caps ""  device ""
    let selector = (\ x -> device_type (reqCapsIn x) == device)
    f selector caps

getKey :: NetAddress -> Int -> IO (Maybe BorutaAuth)
getKey borutaAddr id = do
    let (addr, port) = getAddr borutaAddr
    auth <- handle (curlHandler borutaAddr id) $ Just <$> curlAeson
        parseJSON
        "POST"
        (addr ++ ":" ++ show port ++ "/api/v1/reqs/"
            ++ show id ++ "/acquire_worker")
        [CurlFollowLocation True]
        (Nothing :: Maybe Caps)
    maybe (return Nothing) (\ x -> return $ Just (x{authReqId=id})) auth

-- |Wait for ssh key generating finished on Boruta side.
curlHandler :: NetAddress -> Int -> CurlAesonException -> IO (Maybe BorutaAuth)
curlHandler a id e = threadDelay oneSec >> getKey a id

authMethod a opts
    | force opts = getBusyTargetAuth a
    | otherwise  = getTargetAuth a

-- |Close authorised request. Wait until it will defenitely be closed.
closeAuth addr auth = do
    maybe (return ()) (closeAuth' addr) auth

closeAuth' addr auth = do
    closeRequest addr $ authReqId auth
    waitJobClosed addr auth

waitJobClosed addr auth = do
    req <- getRequestById addr $ authReqId auth
    maybe (return ()) (\ req ->
        if any (\ x -> reqState req == x) activeStatuses
            then threadDelay oneSec >> waitJobClosed addr auth
            else return ()) req

optCloseAuth a opts auth = when (not $ noClose opts) (closeAuth a auth)

-- |Run ssh session for any device which matches specified 'device_type'.
-- Second parameter decide if connect forcely if device is busy.
execAnyDryadConsole :: NetAddress -- ^ Address of requested Boruta made by 'simpleAddress' function
                    -> String     -- ^ Device type
                    -> RequestOptions
                    -> IO ()
execAnyDryadConsole addr x opts = do
    auth <- getDeviceTypeAuth x (authMethod addr opts)
    execDryad addr (sshCmd "") auth
    optCloseAuth addr opts auth

-- |Run ssh session for device specified by its UUID
execSpecifiedDryadConsole :: NetAddress -- ^ Address of requested Boruta made by 'simpleAddress' function
                          -> UUID       -- ^ Device UUID
                          -> RequestOptions
                          -> IO ()
execSpecifiedDryadConsole addr uuid opts = do
    auth <- getSpecifiedTargetAuth uuid (authMethod addr opts)
    execDryad addr (sshCmd "") auth
    optCloseAuth addr opts auth

-- |Execute command on MuxPi
execMuxPi :: NetAddress -- ^ Address of requested Boruta made by 'simpleAddress' function
          -> UUID       -- ^ UUID of the device
          -> String     -- ^ Command
          -> RequestOptions
          -> IO ()
execMuxPi addr uid cmd opts = do
    auth <- getSpecifiedTargetAuth uid (authMethod addr opts)
    execDryad addr (sshCmd cmd) auth
    optCloseAuth addr opts auth

-- |Execute command on the device under test of the Dryad specified by UUID
execDUT :: NetAddress -- ^ Address of requested Boruta made by 'simpleAddress' function
        -> UUID       -- ^ UUID of the device
        -> String     -- ^ Command
        -> RequestOptions
        -> IO ()
execDUT addr uid cmd opts = do
    auth <-  getSpecifiedTargetAuth uid (authMethod addr opts)
    execDryad addr (sshCmd ./"dut_exec.sh " ++ cmd) auth
    optCloseAuth addr opts auth

-- |Push file from host to the MuxPi of Dryad identified by UUID
pushMuxPi :: NetAddress -- ^ Address of requested Boruta made by 'simpleAddress' function
          -> UUID       -- ^ UUID of the device
          -> [FilePath] -- ^ Files to be copied from host
          -> FilePath   -- ^ Destination file name on the MuxPi
          -> RequestOptions
          -> IO ()
pushMuxPi addr uid sources to opts = do
    auth <- getSpecifiedTargetAuth uid (authMethod addr opts)
    Par.mapM_ (\ from -> execDryad addr (scpCmd from to) auth) sources
    optCloseAuth addr opts auth

-- |The same as 'pushMuxPi' but push file to the device under test
pushDUT :: NetAddress -- ^ Address of requested Boruta made by 'simpleAddress' function
        -> UUID       -- ^ UUID of the device
        -> [FilePath] -- ^ Files to be copied from host
        -> FilePath   -- ^ Destination file name on the Device Under Test (DUT)
        -> RequestOptions
        -> IO ()
pushDUT addr uid sources to opts = do
    auth <- getSpecifiedTargetAuth uid (authMethod addr opts)
    Par.mapM_ (\ from -> do
        execDryad addr (scpCmd from $ tmpfile from) auth
        execDryad addr (sshCmd ./ "dut_copyto.sh " ++ tmpfile from ++ " " ++ to)
            auth
        execDryad addr (sshCmd $ "rm " ++ tmpfile from) auth) sources
    optCloseAuth addr opts auth
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
execDryad :: NetAddress -> DryadCmd -> Maybe BorutaAuth -> IO ()
execDryad _ _ Nothing = do
    putStrLn "Use --force option to connect for existing session if you are \
    \sure you know you do."
    putColorBold Red "IMPORTANT: "
    putStr "force connecting to the running job will cause \
    \closing it after your command done (see also --no-close option), so "
    putColorBold Red "YOU MAY BROKE OTHER'S WORK!!!\n"
execDryad borutaAddr f (Just auth) = do
    let addr = netIP borutaAddr
    keyFile <- writeKey auth
    let creds = DryadSSH
                (username auth) addr (port $ authAddr auth) keyFile
    handle h $ callCommand $ f creds
    where
        h :: SomeException -> IO ()
        h _ = return ()

writeKey auth = writeSystemTempFile "boruta-key" (sshKey auth)

-- |Close Boruta request specified by its ID
closeRequest :: NetAddress -> Int -> IO ()
closeRequest borutaAddr id = do
    let (addr, port) = getAddr borutaAddr
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
dutBoot addr uid opts = do
    auth <- getSpecifiedTargetAuth uid (authMethod addr opts)
    execDryad addr (sshCmd ./"dut_boot.sh") auth
    execDryad addr (sshCmd ./"dut_login.sh root") auth
    optCloseAuth addr opts auth

-- |Set specified by UUID dryad in MAINTENANCE mode: Weles will not be able to
-- start tests on it.
setMaintenace :: NetAddress -> UUID -> IO ()
setMaintenace a uuid = setState a (WorkerState "MAINTENANCE") uuid

-- |Set specified by UUID dryad in IDLE mode: it can be used by Weles normally.
setIdle :: NetAddress -> UUID -> IO ()
setIdle a uuid = setState a (WorkerState "IDLE") uuid

-- |Set specified by UUID dryad in specified 'WorkerState' mode.
setState :: NetAddress -> WorkerState -> UUID -> IO ()
setState borutaAddr req uuid = do
    let (addr, port) = getAddr borutaAddr
    (curlAeson
            parseJSON
            "POST"
            (addr ++ ":" ++ show port ++
                "/api/v1/workers/" ++ toString uuid ++ "/setstate")
            [CurlFollowLocation True]
            (Just req)) :: IO ()

-- |Prepend executable path to the string and execute function
infixr 4 ./
(./) :: (String -> a) -> String -> a
f ./ x = f $ "/usr/local/bin/" ++ x

oneSec = 1000000

getAddr x = (netIP x, netPort x)
