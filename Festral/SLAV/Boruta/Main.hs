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

-- |Main module for festral-boruta executable.
module Main (
    main
) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Paths_Festral (version)

import Festral.Config
import Festral.SLAV.Boruta
import Festral.Internal.Files

main = runCmd =<< customExecParser (prefs showHelpOnEmpty)
    (info (helper <*> parseOptsCmd <|> prgVersion)
     (progDesc "festral-boruta - easy access for Boruta's farm devices \
     \testing process.  Copyright (c) 2018 Samsung Electronics Co., Ltd All \
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
        , force     :: Bool
        }

-- |Representation of boruta command options.
data BorutaSubOpt
    = Workers Bool
    | AllRequests Bool
    | Console BorutaConsole Bool
    | CloseRequest Int
    | DryadCmd BorutaCommand
    | Boot String
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
    = Cmd BorutaSubOpt
    | Version Bool
    | None

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

borutaDesc  = "Give access for the device farm of the Boruta and managament \
\devices under test by hands."
borutaExecDesc = "Execute command on dryad or on DUT"
borutaPushDesc = "Push file(s) to the dryad or to the DUT"

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
    <*> switch
        ( long  "force"
        <>short 'f'
        <>help  "Force execute command even if target is busy. WARNING: \
        \Current boruta's job will be closed after command execution, so \
        \you can broke other's work!" )

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
    <*> switch
        (long   "force"
        <>short 'f'
        <>help  "Force connect device if it is already busy. WARNING: Job \
        \will be closed after you close console and it can broke other's \
        \person work!" )

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

runCmd (Version True) = putStrLn $ "festral v." ++ showVersion version
runCmd (Cmd x) = borutaSubCmd x
runCmd _ = putStrLn "Some parameter missed. Run program with --help option \
\to see usage."

borutaSubCmd (Workers True) = show <$> curlWorkers >>= putStrLn
borutaSubCmd (AllRequests True) = show <$> allRequests >>= putStrLn
borutaSubCmd (Console x force) = borutaConsoleCall x force
borutaSubCmd (CloseRequest x) = closeRequest x
borutaSubCmd (DryadCmd x) = dryadCmdCall x
borutaSubCmd (Boot id) = dutBoot id
borutaSubCmd (SetMaintanence id) = setMaintenace id
borutaSubCmd (SetIdle id) = setIdle id
borutaSubCmd _ = runCmd None

dryadCmdCall (BorutaExec cmds opts)
    = (execFromOpts opts) (dryadId opts) (unwords cmds) (force opts)
dryadCmdCall (BorutaPush files out opts)
    = (pushFromOpts opts) (dryadId opts) files out (force opts)

execFromOpts (DryadAction True _ _) = execDUT
execFromOpts _ = execMuxPi

pushFromOpts (DryadAction True _ _) = pushDUT
pushFromOpts _ = pushMuxPi

borutaConsoleCall (ConsoleFromType x) f = execAnyDryadConsole x f
borutaConsoleCall (ConsoleFromUUID x) f = execSpecifiedDryadConsole x f

justPutStrLn :: (Show a, Eq a) => String -> Maybe a -> IO ()
justPutStrLn errMsg x
    | x == Nothing = putStrLn errMsg
    | otherwise = let Just y = x in putStrLn $ (read $ show y :: String)
