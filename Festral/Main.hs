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

main = runCmd =<< customExecParser (prefs showHelpOnEmpty)
    (info (helper <*> parseOptsCmd <|> prgVersion <|> report)
     (progDesc "Festral - unified application for automating of building and \
     \testing process")
    )

data BorutaConsole
    = ConsoleFromUUID String
    | ConsoleFromType String

data DryadAction
    = DryadExec String
    | DryadPush
        { pushIn    :: FilePath
        , pushOut   :: FilePath
        }

data BorutaSubOpt
    = Workers Bool
    | AllRequests Bool
    | Console BorutaConsole
    | CloseRequest Int
    | DryadCmd
        { action :: DryadAction
        , dut    :: Bool
        , dryadId:: String
        }
    | Boot String

data JobOpts
    = WaitJob Int
    | JobSTDOut Bool
    | ListFiles String
    | CancelJob Bool

data WelesSubOpts
    = AllJobs Bool
    | StartJob FilePath
    | JobSubOpt
        { jobOpt :: JobOpts
        , jobId  :: Int
        }

data Command
    = Build
        { config    :: String
        , reposPath :: String
        , outDir    :: String
        , noClean   :: Bool
        }
    | Weles WelesSubOpts
    | Boruta BorutaSubOpt
    | TestControl
        { perfTest  :: FilePath
        , buildPath :: FilePath
        }
    | Server
        { serverPort :: Int }

data ReportType
    = HTML
        { htmlRep       :: Bool
        , templateHTML  :: FilePath
        }
    | TextReport Bool

data Options
    = Cmd Command
    | Version Bool
    | Report
        { rtype     :: ReportType
        , rOutFile  :: FilePath
        }
    | None

parseOptsCmd :: Parser Options
parseOptsCmd = Cmd <$> opts

testDesc    = "Create jobs on remote Weles server with tests defined in .yaml \
\files and process responces with results of its execution. Put results of tests\
\to the directory specified in testLogDir of the ~/.festral.conf configuration \
\file."
buildDesc   = "Build all repositories for all branches described in \
\configuration file. Put results into the directory specified in the buildLogDir\
\field of the ~/.festral.conf configuration file."
borutaDesc   = "Give access for the device farm of the Boruta and managament \
\devices under test by hands."
welesDesc    = "Low-level client for Weles API for accessing and managing jobs \
\by hands."
serverDesc  = "Run local file server for external parts of test process \
\(like remote Weles server) could access needed files such built rpms."

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
        <>help      "Do not clean output of built repositories. This option is \
        \needed if some repositories requires packages from other repositories \
        \were built before." )

reportHTMLParser :: Parser ReportType
reportHTMLParser = HTML
    <$> switch
        (  long     "html-report"
        <> help     "Generate html report to the stdout or to the file if out \
        \option is specified" )
    <*> strOption
        (  long     "file"
        <> metavar  "TEMPLATE_FILE"
        <> short    'f'
        <> value    ""
        <> help     "Use given HTML file for insert report tables into \
        \templated places." )

reportText :: Parser ReportType
reportText = TextReport
    <$> switch
        (  long     "text-report"
        <> help     "Show results of the tests as simple text." )

report :: Parser Options
report = Report
    <$> (reportHTMLParser <|> reportText )
    <*> strOption
        (  long     "out"
        <> metavar  "FILENAME"
        <> short    'o'
        <> value    ""
        <> help     "Output directory for summary report." )

prgVersion :: Parser Options
prgVersion = Version
    <$> switch
        (  long     "version"
        <> short    'v'
        <> help     "Show this program version." )

welesWaitJob :: Parser JobOpts
welesWaitJob = WaitJob
    <$> option auto
        ( long  "when-done"
        <>short 'd'
        <>metavar "TIMEOUT_SECS"
        <>value 0
        <>help  "Wait until queried job done before doing rest and until \
        \TIME_LIMIT is not left. Job is cancelled after time is out. \
        \TIME_LIMIT is in seconds." )

welesJobSTDOUT :: Parser JobOpts
welesJobSTDOUT = JobSTDOut
    <$> switch
        ( long  "job-stdout"
        <>help  "Print standard output and error streams of job." )

welesListFiles :: Parser JobOpts
welesListFiles = ListFiles
    <$> strOption
        ( long  "list-files"
        <>metavar "FILENAME"
        <>value ""
        <>help  "Print content of FILENAME on Weles server of print list of \
        \files if no FILENAME given")


welesCancelJob :: Parser JobOpts
welesCancelJob = CancelJob
    <$> switch
        ( long  "cancel"
        <>short 'c'
        <>help  "Cancel job" )

welesAllJobs :: Parser WelesSubOpts
welesAllJobs = AllJobs
    <$> switch
        ( long  "all"
        <>short 'a'
        <>help  "List all jobs." )

welesStartJob :: Parser WelesSubOpts
welesStartJob = StartJob
    <$> strOption
        ( long  "start-job"
        <>short 's'
        <>metavar "YAML_FILE"
        <>help  "Start new job passing to the Weles YAML_FILE. Returns id of \
        \the created job." )

welesJobOpt :: Parser WelesSubOpts
welesJobOpt = JobSubOpt
    <$> (welesWaitJob <|> welesJobSTDOUT <|> welesListFiles <|> welesCancelJob)
    <*> option auto
        ( long  "job-id"
        <>short 'i'
        <>help  "Id of the job."
        <>metavar "JOB_ID" )

welesopts :: Parser Command
welesopts = Weles
    <$> (welesAllJobs <|> welesStartJob <|> welesJobOpt)

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

dryadExec :: Parser DryadAction
dryadExec = DryadExec
    <$> strOption
        ( long  "exec"
        <>short 'e'
        <>metavar "COMMAND"
        <>help  "Execute command on Dryad" )

dryadPush :: Parser DryadAction
dryadPush = DryadPush
    <$> strOption
        ( long  "push"
        <>short 'p'
        <>metavar "SOURCE_FILE"
        <>help  "Push file from host." )
    <*> strOption
        ( long  "dst"
        <>short 'o'
        <>metavar "DESTINATION_FILE"
        <>help  "Copied file name on target." )

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

borutaCloseRequest :: Parser BorutaSubOpt
borutaCloseRequest = CloseRequest
    <$> option auto
        ( long  "close"
        <>short 'c'
        <>metavar "REQUEST_ID"
        <>help  "Close request specified by its ID" )

borutaDryadCmd :: Parser BorutaSubOpt
borutaDryadCmd = DryadCmd
    <$> (dryadExec <|> dryadPush)
    <*> switch
        ( long  "dut"
        <>help  "Pass command directly to the device under test instead of \
        \MuxPi" )
    <*> strOption
        ( long  "uuid"
        <>short 'u'
        <>metavar "DRYAD_UUID"
        <>help  "UUID of the dryad." )

borutaBoot :: Parser BorutaSubOpt
borutaBoot = Boot
    <$> strOption
        ( long  "boot"
        <>short 'b'
        <>metavar "DRYAD_UUID"
        <>help  "Boot device under test of Dryad dpecified by UUID" )

borutaOpts :: Parser Command
borutaOpts = Boruta <$>
    (  borutaWorkers
    <|>borutaAllRequests
    <|>borutaConsole
    <|>borutaCloseRequest
    <|>borutaDryadCmd
    <|>borutaBoot)

testCtl :: Parser Command
testCtl = TestControl
    <$> strOption
        ( long  "run-test"
        <>short 'r'
        <>metavar "TEST_CONFIG_FILE"
        <>help  "Run tests listed in TEST_CONFIG_FILE for specified by -f \
        \option build directory. Run for all targets listed in \
        \'~/.festral/fresh_builds' file if no target specified." )
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
runCmd (Report x o) = reportCmd x o
runCmd (Cmd x) = subCmd x
runCmd _ = putStrLn "Some parameter missed. Run program with --help option \
\to see usage."

reportCmd (HTML True x) "" = putStrLn =<< reportHTML =<< readNotEmpty x
reportCmd (HTML True x) o = do
    html <- reportHTML =<< readNotEmpty x
    writeFile o html
reportCmd (TextReport True) "" = undefined
reportCmd (TextReport True) o = undefined
reportCmd _ _ = runCmd None

subCmd :: Command -> IO ()
subCmd (Weles x) = welesSubCmd x
subCmd (Boruta x) = borutaSubCmd x
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

    builder <- builderFromFile config
    let cutHere = "-------------------- Result builds -----------------------\n"
    putStrLn =<< (return . ((++) cutHere)) =<<  maybe
        (return "ERROR: Check your configuration JSON: it has bad format.")
        ((unlines <$>).(concat <$>)
            .mapM (\x -> build x (BuildOptions noClean) repos out))
        builder
    where

subCmd (Server port) = runServerOnPort port

welesSubCmd (AllJobs True) = show <$> curlJobs >>= putStrLn
welesSubCmd (StartJob x) = show <$> startJob x >>= putStrLn
welesSubCmd (JobSubOpt x id) = jobCmd x id
welesSubCmd _ = runCmd None

jobCmd (WaitJob 0) id = show <$> getJob id >>= putStrLn
jobCmd (WaitJob x) id = show <$> getJobWhenDone id x >>= putStrLn
jobCmd (JobSTDOut True) id = getJobOut id >>= putStrLn
jobCmd (ListFiles "") id = show <$> getFileList id >>= putStrLn
jobCmd (ListFiles x) id = getJobOutFile id x >>= justPutStrLn "No such job."
jobCmd (CancelJob True) id = cancelJob id
jobCmd _ _ = runCmd None

borutaSubCmd (Workers True) = show <$> curlWorkers >>= putStrLn
borutaSubCmd (AllRequests True) = show <$> allRequests >>= putStrLn
borutaSubCmd (Console x) = borutaConsoleCall x
borutaSubCmd (CloseRequest x) = closeRequest x
borutaSubCmd (DryadCmd a dut id) = dryadCmdCall a dut id
borutaSubCmd (Boot id) = dutBoot id
borutaSubCmd _ = runCmd None

dryadCmdCall (DryadExec cmd) True id = execDUT id cmd
dryadCmdCall (DryadExec cmd) _ id = execMuxPi id cmd
dryadCmdCall (DryadPush from to) True id = pushDUT id from to
dryadCmdCall (DryadPush from to) _ id = pushMuxPi id from to

borutaConsoleCall (ConsoleFromType x) = execAnyDryadConsole x
borutaConsoleCall (ConsoleFromUUID x) = execSpecifiedDryadConsole x

justPutStrLn :: (Show a, Eq a) => String -> Maybe a -> IO ()
justPutStrLn errMsg x
    | x == Nothing = putStrLn errMsg
    | otherwise = let Just y = x in putStrLn $ (read $ show y :: String)

readNotEmpty "" = return ""
readNotEmpty x = readFile x
