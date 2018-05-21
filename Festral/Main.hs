module Main (
    main
) where

import System.Process
import System.IO
import System.Environment
import Festral

main = do
    args <- getArgs
    runCmd args

runCmd :: [String] -> IO ()

runCmd ["all", "jobs"] = show <$> curlJobs >>= putStrLn

runCmd ["get", "job", y] = do
    let id = read y :: Int
    show <$> getJob id >>= putStrLn

runCmd ["get", "job", "when", "done", y] = do
    let id = read y :: Int
    show <$> getJobWhenDone id >>= putStrLn

runCmd ["start", "job", f] = show <$> startJob f >>= putStrLn

runCmd ["list", "job", "files", id] = show <$> getFileList (read id :: Int) >>= putStrLn

runCmd ["get", "job", "file", id, fname] = getJobOutFile (read id :: Int) fname >>= justPutStrLn "No such job."

runCmd ["get", "job", "stdout", id] = getJobOut (read id :: Int) >>= putStrLn

runCmd _ = do
    pname <- getProgName
    putStrLn $ "Usage: \n" 
                ++ pname ++ " all jobs - list all jobs on weles\n"
                ++ pname ++ " get job <id> - show information about job with given id\n"
                ++ pname ++ " get job when done <id> - like 'get job' but waiting until given job finished\n"
                ++ pname ++ " start job <yamlname> - start job described in 'yamlfile' and returns its id\n" 
                ++ pname ++ " list job files <id> - list all output files created by job with given id>\n" 
                ++ pname ++ " get job file <id> <filename> - get content of the file with <filename> of the jod with given id>\n" 
                ++ pname ++ " get job stdout <id> - get standard out and standard error outputs of the job given by id\n" 

justPutStrLn :: (Show a, Eq a) => String -> Maybe a -> IO ()
justPutStrLn errMsg x
    | x == Nothing = putStrLn errMsg
    | otherwise = let Just y = x in putStrLn $ (read $ show y :: String)
