module Main (
    main
) where

import System.Environment
import Festral.Builder.Builder
import Data.Maybe
import System.Directory
import System.IO
import Festral.Files
import Options.Applicative
import Data.Semigroup ((<>))
import Festral.Builder.Reporter

main = runCmd =<< execParser options
    where
        options = info ((opts <|> report <|> prgVersion) <**> helper)
            ( fullDesc
            <>progDesc  "Build all repositories for all branches described in configuration file"
            <>header    "Festral - simple client for tests management using Weles as test server")

data Options = Options
    { config    :: String
    , reposPath :: String
    , outDir    :: String
    }
    | Report
    { htmlReport :: String }
    | Version
    { version   :: Bool}

opts :: Parser Options
opts = Options
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
        <> help     "Output root directory" )

report :: Parser Options
report = Report
    <$> strOption
        (  long     "html-out"
        <> metavar  "DIRECTORY"
        <> help     "Output directory for summary report of the build. Generate report only if this option is specified, otherwise no report will be generated" )

prgVersion :: Parser Options
prgVersion = Version
    <$> switch
        (  long     "version"
        <> short    'v'
        <> help     "Show this program version." )

runCmd :: Options -> IO ()
runCmd (Options config repos out) = do
    freshBuildsFile <- freshBuilds
    writeFile freshBuildsFile ""

    builder' <- builderFromFile config
    if isJust builder'
        then do
            let Just builder = builder'
            mapM_ (\x -> build x repos out) builder
        else
            putStrLn "ERROR: Check your configuration JSON: it has bad format."

runCmd (Report report) = do
    html <- reportHTML
    writeFile report html

runCmd (Version True) = putStrLn $ "festral-build v." ++ progVersion
