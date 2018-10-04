module Main (
    main
) where

import System.Process
import System.IO
import System.Environment
import Festral.SLAV.Weles hiding (info)
import Options.Applicative
import Data.Semigroup ((<>))
import Festral.Tests.Test
import Festral.Files
import Festral.Reporter
import Festral.Builder.Builder
import Data.Maybe
import Paths_Festral (version)
import Data.Version (showVersion)
import Festral.WWW.Server
import Festral.Config
import Festral.SLAV.Boruta

main = runCmd =<< execParser
    (info (helper <*> parseOptsCmd <|> prgVersion <|> report)
     (progDesc "Festral - unified application for automating of building and testing process")
    )

data Command
    = Build
        { config    :: String
        , reposPath :: String
        , outDir    :: String
        , noClean   :: Bool
        }
    | Weles
        { all       :: Bool
        , jobId     :: Int
        , done      :: Int
        , filename  :: FilePath
        , start     :: Bool
        , stdout    :: Bool
        , listFile  :: Bool
        , cancel    :: Bool
        }
    | Boruta
        { workers   :: Bool
        , allReq    :: Bool
        , console   :: Bool
        , devType   :: String
        , deviceId  :: String
        , close     :: Int
        , exec      :: String
        , dut       :: Bool
        , push      :: String
        , bOutFile  :: FilePath
        , boot      :: Bool
        }
    | TestControl
        { perfTest  :: FilePath
        , buildPath :: FilePath
        }
    | Server
        { serverPort :: Int }

data Options
    = Cmd Command
    | Version Bool
    | Report
        { out   :: FilePath
        , rep   :: Bool
        , file  :: FilePath
        }
    | None


parseOptsCmd :: Parser Options
parseOptsCmd = Cmd <$> opts

testDesc    = "Create jobs on remote Weles server with tests defined in .yaml files and process responces with results of its execution. "
            ++"Put results of tests to the directory specified in testLogDir of the ~/.festral.conf configuration file"
buildDesc   = "Build all repositories for all branches described in configuration file. "
            ++"Put results into the directory specified in the buildLogDir field of the ~/.festral.conf configuration file."
borutaDesc   = "Give access for the device farm of the Boruta and managament devices under test by hands."
welesDesc    = "Low-level client for Weles API for accessing and managing jobs by hands."
serverDesc  = "Run local file server for external parts of test process (like remote Weles server) could access needed files such built rpms."

opts :: Parser Command
opts = hsubparser
    ( command "build" (info buildopts (progDesc buildDesc))
    <>command "weles" (info welesopts (progDesc welesDesc))
    <>command "boruta" (info borutaOpts (progDesc borutaDesc))
    <>command "test" (info testCtl (progDesc testDesc))
    <>command "server" (info runServer (progDesc serverDesc))
    )

buildopts :: Parser Command
buildopts = Build
    <$> strOption
        (  long     "config"
        <> metavar  "FILENAME"
        <> short    'c'
        <> help     "Configuration file" )
    <*> strOption
        (  long     "repos"
        <> metavar  "DIRECTORY"
        <> short    'r'
        <> help     "Directory with repositories to build." )
    <*> strOption
        (  long     "out"
        <> metavar  "DIRECTORY"
        <> short    'o'
        <> value    ""
        <> help     "Output root directory" )
    <*> switch
        ( long      "no-clean-builds"
        <>help      "Do not clean output of built repositories. This option is needed if some repositories requires packages from other repositories were built before." )

report :: Parser Options
report = Report
    <$> strOption
        (  long     "out"
        <> metavar  "FILENAME"
        <> short    'o'
        <> value    ""
        <> help     "Output directory for summary report." )
    <*> switch
        (  long     "html-report"
        <> help     "Generate html report to the stdout or to the file if out option is specified" )
    <*> strOption
        (  long     "file"
        <> metavar  "FILENAME"
        <> short    'f'
        <> value    ""
        <> help     "Use given HTML file for insert report tables into templated places." )

prgVersion :: Parser Options
prgVersion = Version
    <$> switch
        (  long     "version"
        <> short    'v'
        <> help     "Show this program version." )


welesopts :: Parser Command
welesopts = Weles
    <$> switch
        ( long  "all"
        <>short 'a'
        <>help  "List all jobs." )
    <*> option auto
        ( long  "job-id"
        <>short 'i'
        <>value (-1)
        <>showDefault
        <>help  "Id of the job to be selected. With no other options list information about this job."
        <>metavar "JOB_ID" )
    <*> option auto
        ( long  "when-done"
        <>short 'd'
        <>metavar "TIME_LIMIT"
        <>value 0
        <>help  "Wait until queried job done before doing rest and until TIME_LIMIT is not left. Job is cancelled after time is out. TIME_LIMIT is in seconds." )
    <*> strOption
        ( long  "filename"
        <>short 'f'
        <>value ""
        <>metavar "FILENAME"
        <>help  ("Give filename to the program. What to to with "
                ++ "it depends on other selected options.") )
    <*> switch
        ( long  "start-job"
        <>short 's'
        <>help  ("Start new job passing to the Weles yaml file set by -f option. "
                 ++ "Returns id of the created job.") )
    <*> switch
        ( long  "job-stdout"
        <>help  "Print standard output and error streams of job set by -i option." )
    <*> switch
        ( long  "list-files"
        <>help  "List all files generated by job set by -i option. If -f option is set print content of that file if it was created by job." )
    <*> switch
        ( long  "cancel"
        <>short 'c'
        <>help  "Cancel job specified by -i option." )

borutaOpts :: Parser Command
borutaOpts = Boruta
    <$> switch
        ( long  "workers"
        <>short 'w'
        <>help  "Show list of workers (registered devices) of the Boruta" )
    <*> switch
        ( long  "all"
        <>short 'a'
        <>help  "Show list of all requests of Boruta" )
    <*> switch
        ( long  "console"
        <>help  "Open SSH console for target specified by -u or by -t option. If device type is specified by -i option open consoly for any device matching this type." )
    <*> strOption
        ( long  "device_type"
        <>value ""
        <>short 't'
        <>metavar "DEVICE_TYPE"
        <>help  "Specify device type of the target" )
    <*> strOption
        ( long  "uuid"
        <>value ""
        <>short 'u'
        <>metavar "DEVICE_UUID"
        <>help  "Specify UUID of the target device" )
    <*> option auto
        ( long  "close"
        <>short 'c'
        <>value (-1)
        <>metavar "REQUEST_ID"
        <>help  "Close request given by ID." )
    <*> strOption
        ( long  "exec"
        <>value ""
        <>metavar "COMMAND"
        <>help  "Execute COMMAND on the MuxPi target specified by UUID with -u option. If --dut option is enabled execute command directly on device under test instead of MuxPi" )
    <*> switch
        ( long "dut"
        <>help "Pass command directly to the device under test instead of MuxPi" )
    <*> strOption
        ( long  "push"
        <>value ""
        <>metavar "FILENAME"
        <>help  "Push file to the MuxPi target specified by UUID with -u option into the location specified by -o option. If --dut option is enabled push directly on device under test instead of MuxPi" )
    <*> strOption
        ( long  "out"
        <>short 'o'
        <>value ""
        <>metavar "FILENAME"
        <>help  "Specify output filename." )
    <*> switch
        ( long  "boot"
        <>short 'b'
        <>help  "Boot device under test dpecified by UUID with -u option" )

testCtl :: Parser Command
testCtl = TestControl
    <$> strOption
        ( long  "run-test"
        <>short 'r'
        <>metavar "TEST_CONFIG_FILE"
        <>help  "Run tests listed in TEST_CONFIG_FILE for specified by -f option build directory. Run for all targets listed in '~/.festral/fresh_builds' file if no target specified." )
    <*> strOption
        ( long  "with-build"
        <>short 'b'
        <>metavar "BUILD_DIR"
        <>value ""
        <>help  "Run test only for this build if defined" )

runServer :: Parser Command
runServer = Server
    <$> option auto
        ( long  "port"
        <>short 'p'
        <>metavar "PORT_NUMPER"
        <>value 8080
        <>help  "Port of the file server to start on" )

runCmd :: Options -> IO ()

runCmd (Version True) = putStrLn $ "festral v." ++ showVersion version
runCmd (Report "" True src) = putStrLn =<< reportHTML =<< readNotEmpty src
runCmd (Report report True src) = do
    html <- reportHTML =<< readNotEmpty src
    writeFile report html
runCmd (Cmd x) = subCmd x

runCmd _ = putStrLn "Some parameter missed. Run program with --help option to see usage."

subCmd :: Command -> IO ()
subCmd (Weles all id done fname start stdout listFile cancel)
    | all = show <$> curlJobs >>= putStrLn
    | cancel = cancelJob id
    | start = show <$> startJob fname >>= putStrLn
    | listFile && fname == "" = show <$> getFileList id >>= putStrLn
    | listFile = getJobOutFile id fname >>= justPutStrLn "No such job."
    | stdout = getJobOut id >>= putStrLn
    | done == 0 && id /= (-1) = show <$> getJob id >>= putStrLn
    | id /= (-1) = show <$> getJobWhenDone id done >>= putStrLn
    | otherwise = runCmd None

subCmd (Boruta workers allReqs console dType dUUID close ex dut push out boot)
    | console && dType /= "" = execAnyDryadConsole dType
    | console && dUUID /= "" = execSpecifiedDryadConsole dUUID
    | allReqs = show <$> allRequests >>= putStrLn
    | workers = show <$> curlWorkers >>= putStrLn
    | boot && dUUID /=  "" = dutBoot dUUID
    | close >= 0 = closeRequest close
    | ex /= "" && dUUID /= "" && dut = execDUT dUUID ex
    | ex /= "" && dUUID /= "" = execMuxPi dUUID ex
    | push /= "" && out /= "" && dUUID /= "" && dut = pushDUT dUUID push out
    | push /= "" && out /= "" && dUUID /= "" = pushMuxPi dUUID push out
    | otherwise = runCmd None

subCmd (TestControl conf "") = do
    lastTestFile <- freshTests
    writeFile lastTestFile ""

    listFile <- freshBuilds
    list <- readFile listFile
    performForallNewBuilds conf list

subCmd (TestControl config fname) = performTestWithConfig config fname

subCmd (Build config repos "" noClean) = getAppConfig >>=
    (\x -> subCmd (Build config repos (buildLogDir x) noClean))
subCmd (Build config repos out noClean) = do
    freshBuildsFile <- freshBuilds
    writeFile freshBuildsFile ""

    builder' <- builderFromFile config
    if isJust builder'
        then do
            let Just builder = builder'
            mapM_ (\x -> build x (BuildOptions noClean) repos out) builder
        else
            putStrLn "ERROR: Check your configuration JSON: it has bad format."

subCmd (Server port) = runServerOnPort port

justPutStrLn :: (Show a, Eq a) => String -> Maybe a -> IO ()
justPutStrLn errMsg x
    | x == Nothing = putStrLn errMsg
    | otherwise = let Just y = x in putStrLn $ (read $ show y :: String)

readNotEmpty "" = return ""
readNotEmpty x = readFile x
