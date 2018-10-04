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
    , deviceType    :: String
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
           ++"    \"device_type\":"   ++ show (deviceType x)  ++ ",\n"
           ++"    \"UUID\":"          ++ show (uuid x)        ++ "\n"
           ++"  }"

instance FromJSON Caps where
    parseJSON = withObject "Caps" $ \o -> do
        addr        <- o .:? "Addr"         .!= ""
        deviceType  <- o .:? "device_type"  .!= ""
        uuid        <- o .:? "UUID"         .!= ""
        return Caps{..}

instance ToJSON Caps where
    toJSON (Caps a d u) = object $ catMaybes
        [ "Addr"        .=. a
        , "device_type" .=. d
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
            (addr ++ ":" ++ port ++ "/api/workers")
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
            (addr ++ ":" ++ port ++ "/api/reqs/")
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
            (addr ++ ":" ++ port ++ "/api/reqs")
            [CurlFollowLocation True]
    return $ fromMaybe [] (decode (BL.pack str) :: Maybe [BorutaRequest])

-- |Return ssh key for session for given by name target. If this target has
-- running session it returns key for it, othervise it opens new request
getTargetAuth :: (BorutaRequest -> Bool) -> Caps -> IO (Maybe BorutaAuth)
getTargetAuth selector caps = do
    requests <- allRequests
    let active = filter selector requests
    id <- if length active == 0
        then createRequest caps
        else return $ Just $ reqID $ head active
    maybe (return Nothing) getKey id

getSpecifiedTargetAuth targetUUID = do
    let caps = Caps "" "" targetUUID
    let selector = (\(BorutaRequestIn _ state caps) ->
            ((uuid caps) == targetUUID)
            && (state == "IN PROGRESS"))
    getTargetAuth selector caps

-- |Get any accessible device of given device_type
getDeviceTypeAuth device = do
    let caps = Caps "" device ""
    let selector = (\(BorutaRequestIn _ state caps) ->
            (state == "IN PROGRESS")
            && (deviceType caps == device))
    getTargetAuth selector caps

getKey :: Int -> IO (Maybe BorutaAuth)
getKey id = do
    (addr, port) <- borutaAddr
    handle curlHandler $ Just <$> curlAeson
        parseJSON
        "POST"
        (addr ++ ":" ++ port ++ "/api/v1/reqs/" ++ show id ++ "/acquire_worker")
        [CurlFollowLocation True]
        (Nothing :: Maybe Caps)

curlHandler :: CurlAesonException -> IO (Maybe BorutaAuth)
curlHandler e = putStrLn ("All targets are busy and no one can be requested"
    ++ " immediately. Try later.") >> return Nothing

-- |Exec ssh session for any device which matches specified device_type
execAnyDryadConsole :: String -> IO ()
execAnyDryadConsole x = execDryad (sshCmd "") =<< getDeviceTypeAuth x

-- |Exec ssh session for device specified by its UUID
execSpecifiedDryadConsole :: String -> IO ()
execSpecifiedDryadConsole x = execDryad (sshCmd "") =<< getSpecifiedTargetAuth x

-- |Execute command on MuxPi
execMuxPi :: String -> String -> IO ()
execMuxPi uid cmd = execDryad (sshCmd cmd) =<< getSpecifiedTargetAuth uid

-- |Execute command on the device under test of the Dryad specified by UUID
execDUT :: String -> String -> IO ()
execDUT uid cmd = execDryad (sshCmd ./"dut_exec.sh " ++ cmd)
    =<<  getSpecifiedTargetAuth uid

-- |Push file from host to the MuxPi of Dryad identified by UUID
pushMuxPi :: String -> FilePath -> FilePath -> IO ()
pushMuxPi uid from to = execDryad (scpCmd from to)
    =<< getSpecifiedTargetAuth uid

-- |Push file from host to the device under test identified by UUID
pushDUT :: String -> FilePath -> FilePath -> IO ()
pushDUT uid from to = do
    auth <- getSpecifiedTargetAuth uid
    execDryad (scpCmd from tmpfile) auth
    execDryad (sshCmd ./ "dut_copyto.sh " ++ tmpfile ++ " " ++ to)
        auth
    execDryad (sshCmd $ "rm " ++ tmpfile) auth
    where
        tmpfile = "/tmp/festral_copied_file"

sshCmd cmd x = "ssh "
    ++ dsUser x ++ "@" ++ dsIp x ++ " -p " ++ show (dsPort x)
    ++ " -i " ++ idFile x ++ " " ++ cmd
scpCmd fname out x = "scp "
    ++ "-i " ++ idFile x ++ " -P " ++ show (dsPort x) ++ " "
    ++ fname ++ " " ++ dsUser x
    ++ "@" ++ dsIp x ++ ":" ++ out

type DryadCmd = (DryadSSH -> String)

-- |This function takes function converting dryad connection data to the command
-- and this function's argument and executes this command.
execDryad :: DryadCmd -> Maybe BorutaAuth -> IO ()
execDryad f auth = do
    (addr, _) <- borutaAddr
    keyFile <- maybe (return "") writeKey auth
    maybe (return ()) (\ auth ->
        let creds = DryadSSH
                (username auth) addr (port $ authAddr auth) keyFile in
        callCommand $ f creds) auth

writeKey auth = writeSystemTempFile "boruta-key" (sshKey auth)

-- |Close Boruta request specified by its ID
closeRequest :: Int -> IO ()
closeRequest id = do
    (addr, port) <- borutaAddr
    handle h $ curlAeson
        parseJSON
        "POST"
        (addr ++ ":" ++ port ++ "/api/v1/reqs/" ++ show id ++ "/close")
        [CurlFollowLocation True]
        (Nothing :: Maybe Caps)
    where
        h :: CurlAesonException -> IO ()
        h _ = putStrLn "Cant access requesteed ID" >> return ()

-- |Boot up device under test specified by UUID
dutBoot uid = execMuxPi uid ./"dut_boot.sh"
            >>execMuxPi uid ./"dut_login.sh root"

-- |Prepend executable path to the string and execute function
infixr 4 ./
(./) :: (String -> a) -> String -> a
f ./ x = f $ "/usr/local/bin/" ++ x
