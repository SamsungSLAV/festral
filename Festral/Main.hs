module Main (
    main
) where

import System.Process
import System.IO
import System.Environment
import Festral.Weles.API hiding (info)
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
import Festral.Weles.Boruta

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
    | Slav
        { all       :: Bool
        , jobId     :: Int
        , done      :: Int
        , filename  :: FilePath
        , start     :: Bool
        , stdout    :: Bool
        , listFile  :: Bool
        , cancel    :: Bool
        , workers   :: Bool
        , req       :: String
        , allReq    :: Bool
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
welesDesc   = "Deprecated. This name is for backward compatibility. See slav."
slavDesc    = "Allow to use SLAV API for accessing and managing Weles's jobs and Boruta's devices by hands."
serverDesc  = "Run local file server for external parts of test process (like remote Weles server) could access needed files such built rpms."

opts :: Parser Command
opts = hsubparser
    ( command "build" (info buildopts (progDesc buildDesc))
    <>command "weles" (info welesopts (progDesc welesDesc))
    <>command "slav" (info welesopts (progDesc slavDesc))
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
welesopts = Slav
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
    <*> switch
        ( long  "workers"
        <>short 'w'
        <>help  "Show list of workers of the Boruta" )
    <*> strOption
        ( long  "request"
        <>short 'r'
        <>value ""
        <>metavar "DEVICE_NAME"
        <>help  "Create new request for Boruta for given worker." )
    <*> switch
        ( long  "all-requests"
        <>help  "Show list of all requests of Boruta" )

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
subCmd (Slav all id done fname start stdout listFile cancel workers req allReqs)
    | all = show <$> curlJobs >>= putStrLn
    | allReqs = show <$> allRequests >>= putStrLn
    | workers = show <$> curlWorkers >>= putStrLn
    | req /= "" = show <$> createRequest req >>= putStrLn
    | cancel = cancelJob id
    | start = show <$> startJob fname >>= putStrLn
    | listFile && fname == "" = show <$> getFileList id >>= putStrLn
    | listFile = getJobOutFile id fname >>= justPutStrLn "No such job."
    | stdout = getJobOut id >>= putStrLn
    | done == 0 && id /= (-1) = show <$> getJob id >>= putStrLn
    | id /= (-1) = show <$> getJobWhenDone id done >>= putStrLn
    | otherwise = runCmd None

subCmd (TestControl conf "") = do
    lastTestFile <- freshTests
    writeFile lastTestFile ""

    listFile <- freshBuilds
    list <- readFile listFile
    performForallNewBuilds conf list

subCmd (TestControl config fname) = performTestWithConfig config fname

subCmd (Build config repos "" noClean) = getAppConfig >>= (\x -> subCmd (Build config repos (buildLogDir x) noClean))
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
