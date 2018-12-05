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

{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

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
    setState,
    Worker (..),
    BorutaRequest(..),
    Caps(..),
    BorutaAuth(..)
) where

import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import           GHC.Generics
import           Data.Aeson
import           Control.Applicative
import           Control.Exception
import           Network.Curl
import           Network.Curl.Aeson
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           System.Process
import           System.IO
import           System.IO.Temp
import           Data.List.Split
import           Control.Exception

import           Festral.Internal.Files
import           Festral.Config

-- |Data type describing Boruta worker
data Worker = Worker
    { workerUUID    :: String
    , state         :: String
    , groups        :: String
    , caps          :: Caps
    } deriving (Generic)

-- |Caps part of the worker
data Caps = Caps
    { addr          :: String
    , deviceType    :: String -- ^Field for back compatibility with older Weles
    , device_type   :: String
    , uuid          :: String
    } deriving (Generic)

-- |Helper datatype for getting request ID from boruta after creating it
data ReqID = ReqID {simpleReqID :: Int} deriving Generic

-- |Requests (recieved and sent) for Boruta
data BorutaRequest
    -- |Responce recieved from Boruta. Contains ID of the created request and
    -- its state.
    = BorutaRequestIn
    { reqID     :: Int
    , reqState  :: String
    , reqCapsIn :: Caps
    }
    -- |Request to be send for Boruta when ask it about creating new request. It
    -- contains needed data: priority, needed times and information about
    -- requested device.
    | BorutaRequestOut
    { deadline  :: String
    , validAfter:: String
    , reqCapsOut:: Caps
    , priority  :: Int
    }
    deriving (Generic)

-- |Representation of the responce of the Boruta which gives information where
-- to connect to the requested device.
data Addr = Addr
    { ip    :: String
    , port  :: Int
    , zone  :: String
    } deriving (Show, Generic)

-- |Responce from Boruta server for request about connection to the opened
-- 'BorutaRequest'. It contains SSH key, username and address for connecting
-- to the MuxPi specified by 'reqID'.
data BorutaAuth = BorutaAuth
    { sshKey    :: String
    , username  :: String
    , authAddr  :: Addr
    } deriving (Generic, Show)

data DryadSSH = DryadSSH
    { dsUser    :: String
    , dsIp      :: String
    , dsPort    :: Int
    , idFile    :: FilePath
    }

instance FromJSON Addr where
    parseJSON = withObject "Addr" $ \o -> do
        ip   <- o .: "IP"
        port <- o .: "Port"
        zone <- o .: "Zone"
        return Addr{..}

instance ToJSON Addr

instance FromJSON BorutaRequest where
    parseJSON = withObject "BorutaRequestIn" $ \o -> do
        reqID       <- o .: "ID"
        reqState    <- o .: "State"
        reqCapsIn   <- o .: "Caps"
        return BorutaRequestIn{..}

instance ToJSON BorutaRequest where
    toJSON (BorutaRequestOut d v caps p) =
        object ["Deadline"      .= d
               ,"ValidAfter"    .= v
               ,"Priority"      .= p
               ,"Caps"          .= caps
               ]

instance Show BorutaRequest where
    show (BorutaRequestIn i s c) =
          "[" ++ show i ++ "," ++ s ++ "," ++ BL.unpack (encode c) ++ "]\n"
    show x = show $ toJSON x

instance Show Caps where
    show x = "  {\n"
           ++"    \"Addr\":"          ++ show (addr x)        ++ ",\n"
           ++"    \"device_type\":"   ++
                if (device_type x == "")
                    then deviceType x
                    else device_type x
           ++ ",\n"
           ++"    \"UUID\":"          ++ show (uuid x)        ++ "\n"
           ++"  }"

instance FromJSON Caps where
    parseJSON = withObject "Caps" $ \o -> do
        addr        <- o .:? "Addr"         .!= ""
        deviceType  <- o .:? "DeviceType"   .!= ""
        device_type  <- o .:? "device_type" .!= ""
        uuid        <- o .:? "UUID"         .!= ""
        return Caps{..}

instance ToJSON Caps where
    toJSON (Caps a old_d d u) = object $ catMaybes
        [ "Addr"        .=. a
        , "device_type" .=. if d == "" then old_d else d
        , "UUID"        .=. u
        ]

-- |Helper function which  allow to make JSON with only not empty fields.
(.=.) x y = if y == "" then Nothing else Just (x .= y)

instance FromJSON ReqID where
    parseJSON = withObject "ReqID" $ \o -> do
        simpleReqID <- o .: "ReqID"
        return ReqID{..}
instance ToJSON ReqID

instance Show Worker where
    show x = "{\n"
          ++ "  \"WorkerUUID\":"  ++ show (workerUUID x)  ++ ",\n"
          ++ "  \"State\":"       ++ show (state x)       ++ ",\n"
          ++ "  \"Groups\":"      ++ show (groups x)      ++ ",\n"
          ++ "  \"Caps\":"        ++ show (caps x)        ++ "\n"
          ++ "}"

instance FromJSON Worker where
    parseJSON = withObject "Worker" $ \o -> do
        workerUUID  <- o .: "WorkerUUID"
        state       <- o .: "State"
        groups      <- o .:? "Groups" .!= ""
        caps        <- o .: "Caps"
        return Worker{..}
instance ToJSON Worker

instance FromJSON BorutaAuth where
    parseJSON = withObject "BorutaAuth" $ \o -> do
        sshKey      <- o .: "Key"
        username    <- o .: "Username"
        authAddr    <- o .: "Addr"
        return BorutaAuth{..}
instance ToJSON BorutaAuth

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
    let request = BorutaRequestOut afterHour now caps priority
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
        handleCurlInt e = putStrLn "Can't create new request" >> return Nothing

-- |List all requests from Boruta
allRequests :: IO [BorutaRequest]
allRequests = do
    (addr, port) <- borutaAddr
    (err, str) <-
        curlGetString
            (addr ++ ":" ++ show port ++ "/api/reqs")
            [CurlFollowLocation True]
    return $ fromMaybe [] (decode (BL.pack str) :: Maybe [BorutaRequest])

-- |Return ssh key for session for given by name target and request ID.
-- If this target has running session it returns key for it,
-- othervise it opens new request
getTargetAuth :: (BorutaRequest -> Bool) -> Caps -> IO (Maybe (BorutaAuth, Int))
getTargetAuth selector caps = do
    requests <- allRequests
    let active = filter (\ x@(BorutaRequestIn _ state _) ->
                (selector x) && (state `elem` ["IN PROGRESS", "WAITING"]))
                requests
    id <- if length active == 0
        then createRequest caps 4 3600
        else do
            putStrLn $ "Target is busy by request with ID "
                ++ show (reqID $ head active)
            return Nothing
    maybe (return Nothing) getKey id

-- |The same as "getTargetAuth" but connect busy session instead of reject
-- request such request.
getBusyTargetAuth :: (BorutaRequest -> Bool) -> Caps
    -> IO (Maybe (BorutaAuth, Int))
getBusyTargetAuth selector caps = do
    requests <- allRequests
    let active = filter (\ x@(BorutaRequestIn _ state _) ->
                (selector x) && state == "IN PROGRESS") requests
    id <- if length active == 0
        then createRequest caps 4 3600
        else return $ Just $ reqID $ head active
    maybe (return Nothing) getKey id


getSpecifiedTargetAuth targetUUID f = do
    let caps = Caps "" "" "" targetUUID
    let selector = (\(BorutaRequestIn _ state caps) ->
            (uuid caps) == targetUUID)
    f selector caps

-- |Get any accessible device of given device_type
getDeviceTypeAuth device f = do
    let caps = Caps ""  "" device ""
    let selector = (\(BorutaRequestIn _ state caps) ->
            (device_type caps == device) || deviceType caps == device)
    f selector caps

getKey :: Int -> IO (Maybe (BorutaAuth, Int))
getKey id = do
    (addr, port) <- borutaAddr
    auth <- handle curlHandler $ Just <$> curlAeson
        parseJSON
        "POST"
        (addr ++ ":" ++ show port ++ "/api/v1/reqs/"
            ++ show id ++ "/acquire_worker")
        [CurlFollowLocation True]
        (Nothing :: Maybe Caps)
    maybe (return Nothing) (\ x -> return $ Just (x,id)) auth

curlHandler :: CurlAesonException -> IO (Maybe BorutaAuth)
curlHandler e = putStrLn "All targets are busy and no one can be requested \
    \immediately. Try later." >> return Nothing

authMethod True = getBusyTargetAuth
authMethod _ = getTargetAuth

closeAuth auth = maybe (return ()) (\ (_,x) -> closeRequest x) auth

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
        -> String   -- ^ Command
        -> Bool     -- ^ Enforce connection
        -> IO ()
execDUT uid cmd f = do
    auth <-  getSpecifiedTargetAuth uid (authMethod f)
    execDryad (sshCmd ./"dut_exec.sh " ++ cmd) auth
    closeAuth auth

-- |Push file from host to the MuxPi of Dryad identified by UUID
pushMuxPi :: String     -- ^ UUID of the device
          -> FilePath   -- ^ File to be copied from host
          -> FilePath   -- ^ Destination file name on the MuxPi
          -> Bool       -- ^ Enforce connection
          -> IO ()
pushMuxPi uid from to f = do
    auth <- getSpecifiedTargetAuth uid (authMethod f)
    execDryad (scpCmd from to) auth
    closeAuth auth

-- |The same as 'pushMuxPi' but push file to the device under test
pushDUT :: String   -- ^ UUID of the device
        -> FilePath -- ^ File to be copied from host
        -> FilePath -- ^ Destination file name on the Device Under Test (DUT)
        -> Bool     -- ^ Enforce connection
        -> IO ()
pushDUT uid from to f = do
    auth <- getSpecifiedTargetAuth uid (authMethod f)
    execDryad (scpCmd from tmpfile) auth
    execDryad (sshCmd ./ "dut_copyto.sh " ++ tmpfile ++ " " ++ to)
        auth
    execDryad (sshCmd $ "rm " ++ tmpfile) auth
    closeAuth auth
    where
        tmpfile = "/tmp/festral_copied_file"

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
execDryad :: DryadCmd -> Maybe (BorutaAuth, Int) -> IO ()
execDryad _ Nothing = do
    putStrLn "Use --force option to connect for existing session if you are \
    \sure you know you do."
    putStrLn "IMPORTANT: force connecting to the running job will cause \
    \closing it after your command done, so YOU MAY BROKE OTHER'S WORK!!!"
execDryad f (Just (auth, id)) = do
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
        h _ = putStrLn "Cant access requesteed ID" >> return ()

-- |Boot up Device Under Test specified by UUID
dutBoot uid = do
    auth <- getSpecifiedTargetAuth uid (authMethod False)
    execDryad (sshCmd ./"dut_boot.sh") auth
    execDryad (sshCmd ./"dut_login.sh root") auth
    closeAuth auth

-- |Helper type for send JSON with WorkerState to the Boruta.
data WorkerState = WorkerState String deriving Generic

instance ToJSON WorkerState where
    toJSON (WorkerState state) = object ["WorkerState" .= state]

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
