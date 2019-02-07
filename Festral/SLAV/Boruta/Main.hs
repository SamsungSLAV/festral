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

-- |Main module for farmer (festral boruta) executable.
module Main (
    main
) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Paths_Festral (version)
import System.Environment
import System.Process
import Data.UUID

import Festral.Config
import Festral.SLAV.Boruta
import Festral.Internal.Files
import Festral.Internal.Logger

main = runCmd =<< customExecParser (prefs showHelpOnEmpty)
    (info (helper <*> parseOptsCmd <|> prgVersion)
     (progDesc "Farmer - part of Festral system for easy access for Boruta's\
     \farm devices.  Copyright (c) 2018-2019 Samsung Electronics Co., Ltd All \
     \Rights Reserved.")
    )

-- |Helper type for segregate opening console by type or by UUID.
data BorutaConsole
    = ConsoleFromUUID String
    | ConsoleFromType String

-- |Helper type for unite push and exec options.
data DryadAction
    = DryadAction
        { dut       :: Bool
        , dryadId   :: String
        , options   :: RequestOptions
        }

-- |Representation of boruta command options.
data BorutaSubOpt
    = Workers Bool
    | AllRequests Bool
    | Console BorutaConsole RequestOptions
    | CloseRequest Int
    | DryadCmd BorutaCommand
    | Boot String RequestOptions
    | SetMaintanence String
    | SetIdle String

-- |Subcommands of Boruta client
data BorutaCommand
    = BorutaExec
        { execArgs  :: [String]
        , execOpts  :: DryadAction
        }
    | BorutaPush
        { pushFiles :: [FilePath]
        , targetFile:: FilePath
        , pushOpts  :: DryadAction
        }

data Options
    = Cmd BorutaSubOpt FilePath
    | Version Bool
    | None

-- |This is invalid path and it is used only for to be showed in help and used
-- as default value. REal path get by 'configFile' function.
defaultConfigPath = "~/" ++ defaultConfigFileName

parseOptsCmd :: Parser Options
parseOptsCmd = Cmd <$>
    (  borutaWorkers
    <|>borutaAllRequests
    <|>borutaConsole
    <|>borutaCloseRequest
    <|>borutaDryadCmd
    <|>borutaBoot
    <|>borutaSetMaintanence
    <|>borutaSetIdle)
    <*> strOption
        (  long     "config"
        <> metavar  "FILENAME"
        <> value    defaultConfigPath
        <> showDefault
        <> help     "Program configuration file." )

borutaDesc  = "Give access for the device farm of the Boruta and managament \
\devices under test by hands."
borutaExecDesc = "Execute command on dryad or on DUT"
borutaPushDesc = "Push file(s) to the dryad or to the DUT"

requestOptions :: Parser RequestOptions
requestOptions = RequestOptions
    <$> switch
        ( long  "force"
        <>short 'f'
        <>help  "Force execute command even if target is busy. WARNING: \
        \Current boruta's request will be closed after command execution \
        \if no --no-close option enabled, so you can broke other's work!" )
    <*> switch
        ( long  "no-close"
        <>short 'n'
        <>help  "Do not close Boruta request after finish." )

borutaSubOpts :: Parser BorutaCommand
borutaSubOpts = hsubparser
    ( command "exec" (info borutaExec (progDesc borutaExecDesc))
    <>command "push" (info borutaPush (progDesc borutaPushDesc))
    )

borutaExec :: Parser BorutaCommand
borutaExec = BorutaExec
    <$> some (argument str (metavar "COMMAND"))
    <*> dryadAction

borutaPush :: Parser BorutaCommand
borutaPush = BorutaPush
    <$> some (argument str (metavar "FILES..."))
    <*> strOption
        (  long     "out"
        <> metavar  "FILENAME"
        <> short    'o'
        <> help     "Target filename" )
    <*> dryadAction

dryadAction :: Parser DryadAction
dryadAction = DryadAction
    <$> switch
        ( long  "dut"
        <>help  "Pass command directly to the device under test instead of \
        \MuxPi" )
    <*> strOption
        ( long  "uuid"
        <>short 'u'
        <>metavar "DRYAD_UUID"
        <>help  "UUID of the dryad." )
    <*> requestOptions

prgVersion :: Parser Options
prgVersion = Version
    <$> switch
        (  long     "version"
        <> short    'v'
        <> help     "Show this program version." )

borutaConsoleUUID :: Parser BorutaConsole
borutaConsoleUUID = ConsoleFromUUID
    <$> strOption
        ( long  "console-uuid"
        <>metavar "DRYAD_UUID"
        <>help  "Open console for MuxPi specified by UUID of Dryad." )

borutaConsoleDevice :: Parser BorutaConsole
borutaConsoleDevice = ConsoleFromType
    <$> strOption
        ( long  "console-device"
        <>metavar "DEVCE_TYPE"
        <>help  "Open console for any MuxPi connected for given \
        \DEVICE_TYPE board" )

borutaWorkers :: Parser BorutaSubOpt
borutaWorkers = Workers
    <$> switch
        (long   "workers"
        <>short 'w'
        <>help  "Show list of workers (registered devices) of the Boruta" )

borutaAllRequests :: Parser BorutaSubOpt
borutaAllRequests = AllRequests
    <$> switch
        (long   "all"
        <>short 'a'
        <>help  "Show list of all requests of Boruta" )

borutaConsole :: Parser BorutaSubOpt
borutaConsole = Console <$> (borutaConsoleUUID <|> borutaConsoleDevice)
    <*> requestOptions

borutaCloseRequest :: Parser BorutaSubOpt
borutaCloseRequest = CloseRequest
    <$> option auto
        ( long  "close"
        <>short 'c'
        <>metavar "REQUEST_ID"
        <>help  "Close request specified by its ID" )

borutaDryadCmd :: Parser BorutaSubOpt
borutaDryadCmd = DryadCmd <$> borutaSubOpts

borutaBoot :: Parser BorutaSubOpt
borutaBoot = Boot
    <$> strOption
        ( long  "boot"
        <>short 'b'
        <>metavar "DRYAD_UUID"
        <>help  "Boot device under test of Dryad dpecified by UUID" )
    <*> requestOptions

borutaSetMaintanence :: Parser BorutaSubOpt
borutaSetMaintanence = SetMaintanence
    <$> strOption
        ( long  "set-maintanence"
        <>short 'm'
        <>metavar "DRYAD_UUID"
        <>help  "Set dryad specified by DRYAD_UUID in the MAINTENANCE mode" )

borutaSetIdle :: Parser BorutaSubOpt
borutaSetIdle = SetIdle
    <$> strOption
        ( long  "set-idle"
        <>short 'i'
        <>metavar "DRYAD_UUID"
        <>help  "Set dryad specified by DRYAD_UUID in the IDLE mode" )

runCmd (Version True) = putStrLn $ "This farmer use festral v."
                        ++ showVersion version
runCmd (Cmd x c) = resolveNetAddr c >>= flip borutaSubCmd x
runCmd _ = getExecutablePath >>= flip callProcess ["--help"]

borutaSubCmd a (Workers True) = show <$> curlWorkers a >>= putStrLn
borutaSubCmd a (AllRequests True) = show <$> allRequests a >>= putStrLn
borutaSubCmd a (Console x force) = borutaConsoleCall a x force
borutaSubCmd a (CloseRequest x) = closeRequest a x
borutaSubCmd a (DryadCmd x) = dryadCmdCall a x
borutaSubCmd a (Boot id opts)
    = uuidOrDie (\id -> dutBoot a id opts) (fromString id)
borutaSubCmd a (SetMaintanence id) = uuidOrDie (setMaintenace a) $ fromString id
borutaSubCmd a (SetIdle id) = uuidOrDie (setIdle a) $ fromString id
borutaSubCmd _ _ = runCmd None

uuidOrDie = maybe (putColorBold Red "Invalid UUID\n")

dryadCmdCall a (BorutaExec cmds opts)
    = uuidOrDie (\ id -> (execFromOpts opts) a id (unwords cmds) (options opts))
    (fromString $ dryadId opts)
dryadCmdCall a (BorutaPush files out opts)
    = uuidOrDie (\ id -> (pushFromOpts opts) a id files out (options opts))
    (fromString $ dryadId opts)

execFromOpts (DryadAction True _ _) = execDUT
execFromOpts _ = execMuxPi

pushFromOpts (DryadAction True _ _) = pushDUT
pushFromOpts _ = pushMuxPi

borutaConsoleCall a (ConsoleFromType x) f = execAnyDryadConsole a x f
borutaConsoleCall a (ConsoleFromUUID x) f
    = uuidOrDie (\ x -> execSpecifiedDryadConsole a x f) (fromString x)

justPutStrLn :: (Show a, Eq a) => String -> Maybe a -> IO ()
justPutStrLn errMsg x
    | x == Nothing = putStrLn errMsg
    | otherwise = let Just y = x in putStrLn $ (read $ show y :: String)

borutaAddr c = NetAddress (borutaIP c) (borutaPort c) (borutaPort c)

resolveNetAddr c
    | c == defaultConfigPath = do
        cfg <- getAppConfig =<< configFile
        return $ simpleAddress (borutaIP cfg) (borutaPort cfg)
    | otherwise = do
        cfg <- getAppConfig c
        return $ simpleAddress (borutaIP cfg) (borutaPort cfg)
