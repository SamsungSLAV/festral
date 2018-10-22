{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

-- |Simple library for test management using Weles as testing server.
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
    Worker (..)
) where

import GHC.Generics
import Data.Aeson
import Control.Applicative
import Festral.Files
import Festral.Config
import Control.Exception
import Network.Curl
import Network.Curl.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL (pack, unpack)
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import System.Process
import System.IO
import System.IO.Temp
import Data.List.Split
import Control.Exception

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

data BorutaRequest = BorutaRequestIn
    { reqID     :: Int
    , reqState  :: String
    , reqCapsIn :: Caps
    }
    | BorutaRequestOut
    { deadline  :: String
    , validAfter:: String
    , reqCapsOut:: Caps
    , priority  :: Int
    }
    deriving (Generic)

data Addr = Addr
    { ip    :: String
    , port  :: Int
    , zone  :: String
    } deriving (Show, Generic)

data BorutaAuth = BorutaAuth
    { sshKey    :: String
    , username  :: String
    , authAddr  :: Addr
    } deriving (Generic, Show)

data DryadSSH = DryadSSH
    { dsUser:: String
    , dsIp  :: String
    , dsPort:: Int
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
workerDeviceType worker = deviceType $ caps worker

borutaAddr = do
    config <- getAppConfig
    return $ (borutaIP config, borutaPort config)

-- |Returns list of workers of boruta under address declared in the application
-- configuration file
curlWorkers :: IO [Worker]
curlWorkers = do
    (addr, port) <- borutaAddr
    (err, str) <-
        curlGetString
            (addr ++ ":" ++ show port ++ "/api/workers")
            [CurlFollowLocation True]
    return $ fromMaybe [] (decode (BL.pack str) :: Maybe [Worker])

-- |Create request for given target defined by `Caps` for 60 minutes from now
-- with priority 4
createRequest :: Caps -> IO (Maybe Int)
createRequest caps = do
    time <- getCurrentTime
    let now = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" time
    let afterHour = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $
                    addUTCTime 3600 time -- Add 1 hour (3600 seconds)
    let request = BorutaRequestOut afterHour now caps 4
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
        then createRequest caps
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
        then createRequest caps
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

-- |Exec ssh session for any device which matches specified device_type. Second
-- parameter decide if connect forcely if device is busy.
execAnyDryadConsole :: String -> Bool -> IO ()
execAnyDryadConsole x f = do
    auth <- getDeviceTypeAuth x (authMethod f)
    execDryad (sshCmd "") auth
    closeAuth auth

-- |Exec ssh session for device specified by its UUID
execSpecifiedDryadConsole :: String -> Bool -> IO ()
execSpecifiedDryadConsole x f = do
    auth <- getSpecifiedTargetAuth x (authMethod f)
    execDryad (sshCmd "") auth
    closeAuth auth

-- |Execute command on MuxPi
execMuxPi :: String -> String -> Bool -> IO ()
execMuxPi uid cmd f = do
    auth <- getSpecifiedTargetAuth uid (authMethod f)
    execDryad (sshCmd cmd) auth
    closeAuth auth

-- |Execute command on the device under test of the Dryad specified by UUID
execDUT :: String -> String -> Bool -> IO ()
execDUT uid cmd f = do
    auth <-  getSpecifiedTargetAuth uid (authMethod f)
    execDryad (sshCmd ./"dut_exec.sh " ++ cmd) auth
    closeAuth auth

-- |Push file from host to the MuxPi of Dryad identified by UUID
pushMuxPi :: String -> FilePath -> FilePath -> Bool -> IO ()
pushMuxPi uid from to f = do
    auth <- getSpecifiedTargetAuth uid (authMethod f)
    execDryad (scpCmd from to) auth
    closeAuth auth

-- |Push file from host to the device under test identified by UUID
pushDUT :: String -> FilePath -> FilePath -> Bool -> IO ()
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
    callCommand $ f creds

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

-- |Boot up device under test specified by UUID
dutBoot uid = do
    auth <- getSpecifiedTargetAuth uid (authMethod False)
    execDryad (sshCmd ./"dut_boot.sh") auth
    execDryad (sshCmd ./"dut_login.sh root") auth
    closeAuth auth

-- |Prepend executable path to the string and execute function
infixr 4 ./
(./) :: (String -> a) -> String -> a
f ./ x = f $ "/usr/local/bin/" ++ x
