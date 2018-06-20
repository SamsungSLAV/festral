module Main (
    main
) where

import System.Process
import System.IO
import System.Environment
import Festral.Weles.API
import Options.Applicative
import Data.Semigroup ((<>))
import Festral.Tests.Test
import System.Directory

main = runCmd =<< execParser options
    where
        options = info (opts <**> helper)
            ( fullDesc
            <>progDesc  "Create jobs on remote Weles server with tests defined in .yaml files and process responces with results of its execution."
            <>header    "Festral - simple client for tests management using Weles as test server" )

data Options = Options
    { all       :: Bool
    , jobId     :: Int
    , done      :: Bool
    , filename  :: String
    , start     :: Bool
    , stdout    :: Bool
    , listFile  :: Bool
    , perfTest  :: Bool
    } | None

configFile =do
    home <- getHomeDirectory 
    return $ home ++ "/.festral.conf"

buildListFile = do
    x <- getHomeDirectory
    createDirectoryIfMissing False $ x ++ "/.festral"
    return $ x ++ "/.festral/fresh_builds"

opts :: Parser Options
opts = Options
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
    <*> switch
        ( long  "when-done"
        <>short 'd'
        <>help  "Wait until queried job done before doing rest." )
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
        ( long  "run-test"
        <>short 'r'
        <>help  "Run test for specified by -f option build directory. Run for all targets listed in '~/.fresh_builds' file if no target specified." )

runCmd :: Options -> IO ()

runCmd (Options _ _ _ "" _ _ _ True) = do
    config <- configFile
    listFile <- buildListFile
    list <- readFile listFile
    performForallNewBuilds config list
    writeFile listFile ""

runCmd (Options _ _ _ fname _ _ _ True) = configFile >>= (\x -> performTestWithConfig x fname)

runCmd (Options True _ _ _ _ _ _ _) = show <$> curlJobs >>= putStrLn

runCmd (Options _ (-1) _ ("") True _ _ _) = runCmd None

runCmd (Options _ (-1) _ f True _ _ _) = show <$> startJob f >>= putStrLn

runCmd (Options _ (-1) _ _ _ _ _ _) = putStrLn "Missing id of the quered job. Use -i option for set it."

runCmd (Options _ id _ ("") _ _ True _) = show <$> getFileList id >>= putStrLn

runCmd (Options _ id _ fname _ _ True _) = getJobOutFile id fname >>= justPutStrLn "No such job."

runCmd (Options _ id _ _ _ True _ _) = getJobOut id >>= putStrLn

runCmd (Options _ id True _ _ _ _ _) = show <$> getJobWhenDone id >>= putStrLn

runCmd (Options _ id False _ _ _ _ _) = show <$> getJob id >>= putStrLn

runCmd _ = putStrLn "Some parameter missed. Run program with --help option to see usage."


justPutStrLn :: (Show a, Eq a) => String -> Maybe a -> IO ()
justPutStrLn errMsg x
    | x == Nothing = putStrLn errMsg
    | otherwise = let Just y = x in putStrLn $ (read $ show y :: String)
