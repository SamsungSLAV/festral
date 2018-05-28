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

-- |Build target located in the first path and put meta file to the directory tree with given in seconf path root directory
build :: Build -> FilePath -> FilePath -> IO ()
build build wdir outdir = do
    (logfile, _) <- openTempFile "/tmp" "build.log"
    buildWithLog logfile (buildCmd build) wdir
    parser <- getParser (buildResParser build) logfile
    meta <- parse parser
    let outDirName = outdir ++ "/"  ++ buildTime meta ++ "_" ++ hash meta
    createDirectory outDirName
    toFile meta (outDirName ++ "/Meta.txt")

getParser :: String -> FilePath -> IO (Parser a)
getParser "GBS" f = do
    p <- fromFile f
    return $ GBS p
getParser exec f = do
    p <- fromFile f
    let p' = setExec exec p
    return $ Own p'

buildWithLog fname cmd wdir = do
    callCommand $ "cd " ++ wdir ++ " ; " ++ cmd ++ " > " ++ fname
