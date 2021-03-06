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

module Main (
    main
) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Control.Concurrent
import Paths_Festral (version)
import System.Environment
import System.Process
import Data.Maybe
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LB

import Festral.WWW.Server
import Festral.Config
import Festral.SLAV.Weles hiding (info, APIVersion(..))
import Festral.Tests.Test
import Festral.Tests.Data
import Festral.Internal.Files
import Festral.Internal.Preprocessor
import Festral.Reporter
import Festral.Builder.Builder (builderFromFile, build, BuildOptions(..))

main = runCmd =<< customExecParser (prefs showHelpOnEmpty)
    (info (helper <*> parseOptsCmd <|> prgVersion)
     (progDesc "Festral - unified application for automating of building and \
     \testing process.  Copyright (c) 2018-2019 Samsung Electronics Co., \
     \Ltd. All Rights Reserved.")
    )

-- |Helper type for unify all Weles options which use job ID parameter.
data JobOpts
    = WaitJob Int
    | JobSTDOut Bool
    | ListFiles Bool String
    | CancelJob Bool
    | JobYaml   Bool

-- |Representation of the weles command options.
data WelesSubOpts
    = AllJobs Bool
    | StartJob FilePath
    | CloseAllJobs Bool
    | JobSubOpt
        { jobOpt :: JobOpts
        , jobId  :: Int
        }
    | ApiVersion Bool

-- |Type which is root of all program commands tree.
data Command
    = Build
        { config    :: FilePath
        , reposPath :: FilePath
        , noClean   :: Bool
        , outFile   :: FilePath
        }
    | Weles WelesSubOpts
    | TestControl
        { perfTest  :: FilePath
        , outTestRes:: FilePath
        , buildPaths:: [FilePath]
        }
    | Server
        { serverPort        :: Int
        , fileServerOnly    :: Bool
        , sharedPath        :: FilePath
        }
    | Report
        { rtype     :: ReportType
        , rOutFile  :: FilePath
        , rPaths    :: [FilePath]
        }
    | SyntaxCheck
        { fname         :: FilePath
        , check_repo    :: String
        , check_yaml    :: FilePath
        , check_parser  :: String
        , check_name    :: String
        , check_timeout :: Int
        , check_runTTL  :: Int
        , check_target  :: String
        , check_outFile :: FilePath
        }

-- |Helper type for segregating report command options.
data ReportType
    = HTML
        { htmlRep       :: Bool
        , templateHTML  :: FilePath
        }
    | TextReport Bool String
    | TestResults Bool
    | AgingTest Bool
    | TestSummary Bool

-- |Options of the program called without commands (festral entery point).
data Options
    = Cmd Command FilePath
    | Version Bool
    | None

-- |This path is not valid because tilda is not expanded to the home directory
-- this way. This path is used only for show in the help where is it. For get
-- valid path use 'configFile' function.
defaultConfigPath = "~/" ++ defaultConfigFileName

parseOptsCmd :: Parser Options
parseOptsCmd = Cmd
    <$> opts
    <*> strOption
        (  long     "config"
        <> metavar  "FILENAME"
        <> value    defaultConfigPath
        <> showDefault
        <> help     "Configuration file of the application." )

testDesc    = "Create jobs on remote Weles server with tests defined in .yaml \
\files and process responces with results of its execution. Put results of \
\tests to the directory specified in testLogDir of the configuration file \
\configuration file."
buildDesc   = "Build all repositories for all branches described in \
\configuration file. Put results into the directory specified in the \
\buildLogDir field of the configuration file."
welesDesc   = "Low-level client for Weles API for accessing and managing jobs \
\by hands."
serverDesc  = "Run local file server for external parts of test process \
\(like remote Weles server) could access needed files such built rpms."
reportDesc  = "Create different types of reports based on test and build \
\results."
syntaxDesc  = "Check syntax of test case scenario file. Festral test case \
\scenario files must have .ftc extension, othervise it will be interpreted \
\as raw YAML file supported by Weles."

opts :: Parser Command
opts = hsubparser
    ( command "build" (info buildopts (progDesc buildDesc))
    <>command "weles" (info welesopts (progDesc welesDesc))
    <>command "test" (info testCtl (progDesc testDesc))
    <>command "server" (info runServer (progDesc serverDesc))
    <>command "report" (info report (progDesc reportDesc))
    <>command "check" (info syntaxCheck (progDesc syntaxDesc))
    )

buildopts :: Parser Command
buildopts = Build
    <$> strOption
        (  long     "build-config"
        <> metavar  "FILENAME"
        <> short    'b'
        <> help     "Configuration file" )
    <*> strOption
        (  long     "repos"
        <> metavar  "DIRECTORY"
        <> short    'r'
        <> help     "Directory with repositories to build." )
    <*> switch
        ( long      "no-clean-builds"
        <>help      "Do not clean output of built repositories. This option is \
        \needed if some repositories requires packages from other repositories \
        \were built before." )
    <*> strOption
        ( long      "out"
        <>short     'o'
        <>value     ""
        <>metavar   "FILENAME"
        <>help      "Write names of the made builds into the file." )

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
        <> help     "Show results of the tests as formatted by user text." )
    <*> strOption
        (  long     "format"
        <> short    'f'
        <> value    "%r[%B] Build: %s Test: %R"
        <> metavar  "FORMAT"
        <> showDefault
        <> help     "Format specifiers are: %b - board, %t - build type\
        \, %c - commit name, %T - build time, %C - toolchain, %u - builder \
        \username, %s - build status, %h - build hash, %o - build output \
        \directory, %r - name of the repository, %B - brunch name, %l - tester\
        \login, %L - tester name, %e - test time, %n - test name, %S - test\
        \status, %d - device name, %R - pass rating passed/all, %A - show \
        \aging test result in format deviations/total iterations. This value \
        \is counted by repeating in test log same test names and its results. \
        \% or %% - insert % character." )

reportTestRes :: Parser ReportType
reportTestRes = TestResults
    <$> switch
        (  long     "test-result"
        <> help     "Show results of given in arguments tests if it was \
        \performed and parsed successfully." )

reportAgingTest :: Parser ReportType
reportAgingTest = AgingTest
    <$> switch
        (  long     "aging-json"
        <> help     "Show full results of the aging tests given by test IDs \
        \passed in arguments. Returned results are in JSON format." )

reportSummary :: Parser ReportType
reportSummary = TestSummary
    <$> switch
        (  long     "summary"
        <> short    's'
        <> help     "show summary of all performed tests passed as arguments" )

report :: Parser Command
report = Report
    <$> (reportHTMLParser
        <|> reportText
        <|> reportTestRes
        <|> reportAgingTest
        <|> reportSummary)
    <*> strOption
        (  long     "out"
        <> metavar  "FILENAME"
        <> short    'o'
        <> value    ""
        <> help     "Output directory for summary report." )
    <*> many (argument str
        (  metavar  "TEST_NAMES..."
        <> help     "Test and build names."))

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

welesJobYaml :: Parser JobOpts
welesJobYaml = JobSTDOut
    <$> switch
        ( long  "testcase"
        <>short 't'
        <>help  "Print YAML with testcase of requested job." )

welesListFiles :: Parser JobOpts
welesListFiles = ListFiles
    <$> switch
        ( long  "list-files"
        <>help  "List files available on the Weles server. If -f option \
        \specified, show content of this file.")
    <*> strOption
        ( long  "file"
        <>short 'f'
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

welesCloseAllJobs :: Parser WelesSubOpts
welesCloseAllJobs = CloseAllJobs
    <$> switch
        ( long  "cancel-all"
        <>help  "Cancel all not done jobs." )

welesAPIVersion :: Parser WelesSubOpts
welesAPIVersion = ApiVersion
    <$> switch
        ( long  "api-version"
        <>help  "Show version of the Weles API" )

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
    <$> (welesWaitJob
        <|> welesJobSTDOUT
        <|> welesListFiles
        <|> welesCancelJob
        <|> welesJobYaml)
    <*> option auto
        ( long  "job-id"
        <>short 'i'
        <>help  "Id of the job."
        <>metavar "JOB_ID" )

welesopts :: Parser Command
welesopts = Weles
    <$> (welesAllJobs
        <|> welesStartJob
        <|> welesJobOpt
        <|> welesCloseAllJobs
        <|> welesAPIVersion)

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
        ( long  "out"
        <>short 'o'
        <>metavar "FILENAME"
        <>value ""
        <>help  "Write names of the performed tests into the file." )
    <*> many (argument str (metavar "BUILD_DIRS..."))

runServer :: Parser Command
runServer = Server
    <$> option auto
        ( long  "port"
        <>short 'p'
        <>metavar "PORT_NUMPER"
        <>value (-1)
        <>help  "Port of the file server to start on" )
    <*> switch
        ( long  "fileserver-only"
        <>help  "Run server in file server only mode. It means that server \
        \will show file tree just under port it listen on, without files/ \
        \prefix. In this mode HTTP API is disabled." )
    <*> argument str
        ( metavar   "SHARED_DIR"
        <>value     ""
        <>help      "Directory to be shared by fileserver. If not set, get\
        \ it from configuration file.")

syntaxCheck :: Parser Command
syntaxCheck = SyntaxCheck
    <$> argument str (metavar "TESTCASE_FILE")
    <*> strOption
        ( long  "repo"
        <>short 'r'
        <>metavar "REPOSITORY_NAME"
        <>value "test"
        <>help  "Set name of the repository to REPOSITORY_NAME while check\
        \ test scenario (%repo field in .ftc files)." )
    <*> strOption
        ( long  "yaml"
        <>short 'y'
        <>metavar "YAML_PATH"
        <>value "test"
        <>help  "Set path of the yaml file to YAML_PATH while check\
        \ test scenario (%yaml field in .ftc files)." )
    <*> strOption
        ( long  "parser"
        <>short 'p'
        <>metavar "PARSER_NAME"
        <>value "test"
        <>help  "Set test log parser name or binary path to PARSER_NAME\
        \ while check test scenario (%parser field in .ftc files)." )
    <*> strOption
        ( long  "name"
        <>short 'n'
        <>metavar "TEST_NAME"
        <>value "test"
        <>help  "Set test name to TEST_NAME while check test scenario\
        \ (%name field in .ftc files)." )
    <*> option auto
        ( long  "timeout"
        <>short 't'
        <>metavar "JOB_TIMEOUT"
        <>value 10
        <>help  "Set test job timeout to JOB_TIMEOUT while check test scenario")
    <*> option auto
        ( long  "runTTL"
        <>short 'l'
        <>metavar "RUN_TTL"
        <>value 10
        <>help  "Set test job time to live after test was started to RUN_TTL\
        \ while check test scenario")
    <*> strOption
        ( long  "target"
        <>short 'd'
        <>metavar "TARGET_DEVICE"
        <>value "test"
        <>help  "Set target name name to TARGET_DEVICE while check test\
        \ scenario (%target field in .ftc files)." )
    <*> strOption
        ( long  "output-file"
        <>short 'o'
        <>metavar "TEST_LOG_FILE"
        <>value "test.log"
        <>help  "Name of the file where test results are put on DUT." )

runCmd :: Options -> IO ()
runCmd (Version True) = putStrLn $ "festral v." ++ showVersion version
runCmd (Cmd x c) = resolvedAppConfig c >>= subCmd x
runCmd _ = do
    p <- getExecutablePath
    args <- getArgs
    callProcess p $ args ++ ["--help"]

defaultIds = do
    testFile <- freshTests
    tests <- safeReadFile testFile
    buildFile <- freshBuilds
    builds <- safeReadFile buildFile
    return $ notEmptyEnteries $ builds ++ "\n" ++ tests

reportCmd (HTML True x) o [] c = do
    args <- defaultIds
    if length args <= 0
        then putStrLn "Nothing to do"
        else reportCmd (HTML True x) o args c

reportCmd (HTML True x) o args c = do
    html <- (\ x -> reportHTML c x args) =<< (readNotEmpty x)
    writeOut o $ html

reportCmd (TextReport True format) o [] c = do
    args <- defaultIds
    if length args <= 0
        then putStrLn "Nothing to do"
        else reportCmd (TextReport True format) o args c

reportCmd (TextReport True format) o args c =
    writeOut o =<< unlines <$> formatTextReport c format args

reportCmd (TestResults True) o args c =
    writeOut o =<< unlines <$> catMaybes <$> mapM (getTestResults c) args

reportCmd (AgingTest True) o args c =
    writeOut o =<< LB.unpack
                <$> encode <$> fmap aging <$> mapM (testReport c) args

reportCmd (TestSummary True) o args c = do
    s <- summary c args
    let out = "Total: "++ show (allTests s)
           ++ "\nTotal passed: " ++ show (passed s)
           ++ "\nTotal test groups: " ++ show (allGroups s)
           ++ "\nExecuted groups: " ++ show (doneGroups s)
    writeOut o out

reportCmd _ _ _ _ = runCmd None

subCmd :: Command -> AppConfig -> IO ()
subCmd (Weles x) c = welesSubCmd (welesAddr c) x
subCmd (Report x o p) c = reportCmd x o p c
subCmd (TestControl conf out []) appCfg = do
    forkIO $ runServerOnPort appCfg
    lastTestFile <- freshTests
    writeFile lastTestFile ""

    listFile <- freshBuilds
    list <- safeReadFile listFile
    outs <- performForallNewBuilds appCfg conf $ notEmptyEnteries list
    writeOut out $ unlines outs

subCmd (TestControl config out fnames) appCfg = do
    forkIO $ runServerOnPort appCfg
    outs <- performForallNewBuilds appCfg config fnames
    writeOut out $ unlines outs

subCmd (Build config repos noClean outFile) appCfg = do
    freshBuildsFile <- freshBuilds
    writeFile freshBuildsFile ""

    builder <- builderFromFile config
    (writeOut outFile) =<<  maybe
        (return "ERROR: Check your configuration JSON: it has bad format.")
        ((fmap (unlines . concat))
            . mapM (\x -> build x (BuildOptions noClean) repos
            (buildLogDir appCfg)))
        builder

subCmd (Server (-1) mode "") c = (serverType mode) c
subCmd (Server port mode "") c = (serverType mode) c{webPagePort=port}
subCmd (Server port mode path) c
    = (serverType mode) c{webPagePort=port, serverRoot=path}

subCmd (SyntaxCheck f r y p n t l d o) c = readFile f
    >>= preprocess (TestUnit (TestConfig r y p n t l [d] o) d)
    >>= putStrLn

serverType True = runFileServerOnly
serverType _ = runServerOnPort

welesSubCmd a (AllJobs True) = show <$> curlJobs a >>= putStrLn
welesSubCmd a (CloseAllJobs True) = cancelAll a
welesSubCmd a (ApiVersion True) = print =<< getAPIVersion a
welesSubCmd a (StartJob x) = show <$> startJob a x >>= putStrLn
welesSubCmd a (JobSubOpt x id) = jobCmd a x id
welesSubCmd _ _ = runCmd None

jobCmd a (WaitJob 0) id = show <$> getJob a id >>= putStrLn
jobCmd a (WaitJob x) id = show <$> getJobWhenDone a id (JobParameters x x)
    >>= putStrLn
jobCmd a (JobSTDOut True) id = getJobOut a id >>= putStrLn
jobCmd a (ListFiles True "") id = show <$> getFileList a id >>= putStrLn
jobCmd a (ListFiles True x) id = getJobOutFile a id x
    >>= justPutStrLn "No such job."
jobCmd a (CancelJob True) id = cancelJob a id
jobCmd a (JobYaml True) id = getJobYaml a id >>= print
jobCmd _ _ _ = runCmd None

justPutStrLn :: (Show a, Eq a) => String -> Maybe a -> IO ()
justPutStrLn errMsg x
    | x == Nothing = putStrLn errMsg
    | otherwise = let Just y = x in putStrLn $ (read $ show y :: String)

readNotEmpty "" = return ""
readNotEmpty x = safeReadFile x

cutHere = "-------------------- Result outputs -----------------------\n"

writeOut "" = (\ x -> putStrLn $ cutHere ++ x)
writeOut x = writeFile x

notEmptyEnteries = filter ((/=) "") . lines

welesAddr c =  NetAddress (welesIP c) (welesPort c) (welesFilePort c)

resolvedAppConfig c
    | c == defaultConfigPath = configFile >>= getAppConfig
    | otherwise = getAppConfig c
