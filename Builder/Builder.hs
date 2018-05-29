{-# LANGUAGE DeriveGeneric #-}

module Builder.Builder (
    builderFromFile,
    build
) where

import Builder.Meta
import Builder.OwnParser
import Builder.GBSParser
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as LB
import System.Process
import System.IO
import System.Directory
import System.Environment
import System.Exit
import Control.Exception

data Build = Build
    { buildName         :: String
    , buildCmd          :: String
    , buildRepo         :: String
    , buildResParser    :: String
    } deriving (Show, Generic)

instance FromJSON Build
instance ToJSON Build

data Parser a = GBS GBSParser | Own OwnParser deriving Show
instance MetaParser (Parser a) where
    parse (GBS x) = parse x
    parse (Own x) = parse x

    fromFile f = do
        p <- fromFile f
        return $ GBS p

builderFromFile :: FilePath -> IO (Maybe [Build])
builderFromFile fname = do
    file <- LB.readFile fname
    return $ decode file

-- |Build target located in the first path + build name and put meta file to the directory tree with given in seconf path root directory
build :: Build -> FilePath -> FilePath -> IO ()
build build wdir outdir = do
    (logfile, _) <- openTempFile "/tmp" "build.log"
    let srcDir = wdir ++ "/" ++ buildName build
    buildWithLog logfile (buildCmd build) srcDir
    parser <- getParser (buildResParser build) logfile
    meta <- parse parser
    let outDirName = outdir ++ "/" ++ hash meta ++ "_" ++ buildTime meta
    createDirectory outDirName
    renameFile logfile (outDirName ++ "/build.log")
    toFile meta (outDirName ++ "/meta.txt")

getParser :: String -> FilePath -> IO (Parser a)
getParser "GBS" f = do
    p <- fromFile f
    return $ GBS p
getParser exec f = do
    p <- fromFile f
    let p' = setExec exec p
    return $ Own p'

buildWithLog fname cmd wdir = do
    catch (callCommand $ "cd " ++ wdir ++ " ; " ++ cmd ++ " | tee " ++ fname) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn "Build failed"

main = do
    args <- getArgs
    let res = if (length args) /= 3
        then putStrLn "Usage: festral-build <config json> <repositoy location> <output directory>"
        else do
            let [cfg, repodir, outdir] = args
            Just builder <- builderFromFile cfg
            mapM_ (\x -> build x repodir outdir) builder
    res        
