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

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- |Module for building process managament. It get information about build
-- targets
-- from given .json file formatted as follow:
--
-- @
-- [
--  {"buildName": "name of the project to build",
--   "buildCmd" : "command used to build, e.g. gbs build -A armv7l ...",
--   "buildRepo": "origin repusitory address of the project",
--   "buildResParser" : "name of the built-in parsers (\"GBS\") or path to the
--   own binary parser, see below",
--   "branch":["master", "other branch", "etc"]
--   },
--   another build targets ...
-- ]
-- @
--
-- Parser is some script or binary which generates meta.txt file from output of
-- your 'buildCmd' command.
--
-- meta.txt file has format:
--
-- @
--  BOARD=name of the board or arch of target
--  BUILD_TYPE= debug or somthing else, I don't know for what it is
--  COMMIT=name of the built commit
--  BUILD_TIME=build time in format YYYYMMDDHHMMSS
--  TOOLCHAIN=name of toolchain used for build
--  BUILDER=username of builder
--  BUILD_STATUS=result of build (SUCCEED and FAILED are known, but may be there
--  are other ones)
--  BUILD_HASH=hash of the build
--  REPO_NAME=name of the built repository
--  BRANCH=name of the built branch
--  OUT_DIR=directory where build command put output files
--
--  #In the tests directories meta.txt has additional fields:
--  TESTER=login of the tester
--  TESTER_NAME=name of the tester
--  TEST_STATUS=Exit status of the test executing. If test was performed and
--  logs was made, it contains COMPLETE, but it does not mean that test was
--  passed.
--  TEST_TIME=time where test was performed
--  TEST_NAME=Name of the test
--  TEST_DEVICE=name of the device which test was performed on.
-- @
--
-- Parser script must gets output of the 'buildCmd' from its 'stdin' and writes
-- meta file to the 'stdout'
module Festral.Builder.Builder (
    builderFromFile,
    build,
    BuildOptions (..),
    Build(..)
) where

import Data.Aeson
import qualified GHC.Generics as G
import qualified Data.ByteString.Lazy.UTF8 as LBU
import System.Process
import System.IO
import System.Directory
import System.Environment
import System.Exit
import Control.Exception
import System.Posix.User
import Data.List
import Data.List.Split
import Control.Monad (when)
import System.File.Tree (getDirectory', copyTo_)
import Data.Maybe

import Festral.Internal.Files
import Festral.Meta
import Festral.Builder.OwnParser
import Festral.Builder.GBSParser

-- |Build structure which represents config json format.
data Build = Build
    { buildName         :: String
    , buildCmd          :: String
    , buildRepo         :: String
    , buildResParser    :: String
    , branches          :: [String]
    } deriving (Show, G.Generic)

instance FromJSON Build
instance ToJSON Build

-- |Options for build process
data BuildOptions = BuildOptions { noCleanRes :: Bool}

-- |Gets builder object from given configuration json.
builderFromFile :: FilePath -> IO (Maybe [Build])
builderFromFile fname = do
    file <- safeReadFile fname
    return $ decode $ LBU.fromString file

-- |Build target located at the given path and put meta file to
-- the directory tree given from "Festral.Config" from 'buildLogDir' field of
-- the configuration file.
-- Returns list of names of built directories.
build :: Build          -- ^ Build configuration
      -> BuildOptions   -- ^ Build options
      -> FilePath       -- ^ Root directory of cloned repositories
      -> FilePath       -- ^ Output directory ('buildLogDir' usually)
      -> IO [String]    -- ^ List of the names of the performed builds
build build opts wdir outdir = do
    cloneRepo wdir build
    let srcDir = wdir ++ "/" ++ buildName build
    catMaybes <$> mapM (\x -> handle badFileMaybe $
        withCurrentDirectory srcDir (buildOne x opts outdir build))
        (branches build)
    where
        badFileMaybe :: SomeException -> IO (Maybe String)
        badFileMaybe _ = return Nothing

buildOne branch opts outdir build = do
    (logfile, loghandle) <- getTemporaryDirectory >>=
        flip openTempFileWithDefaultPermissions "build.log"
    hSetEncoding loghandle latin1
    prepareRepo branch
    buildWithLog logfile (buildCmd build)
    parse <- getParser (buildResParser build) loghandle
    meta <- parse build branch
    writeBuildOut logfile outdir opts meta

writeBuildOut _ _ _ Nothing = return Nothing
writeBuildOut logfile outdir opts (Just meta) = do
    let out = hash $>> meta ++ "_B" ++ buildTime $>> meta
    let outDirName = outdir ++ "/" ++ out
    let getBuildOut = if noCleanRes opts then copyDirectory else renameDirectory

    createDirectoryIfMissing True outDirName
    toFile meta (outDirName ++ "/meta.txt")
    -- create directory with built files by copy/move results to it
    catch (getBuildOut (outDir $>> meta) (outDirName ++ "/build_res")) handler
    catch (renameFile logfile (outDirName ++ "/build.log"))
        (copyHandler logfile (outDirName ++ "/build.log"))
    bLogFile <- freshBuilds
    appendFile bLogFile (out ++ "\n")

    resFiles <- handle badDir $
        getDirectoryContents (outDirName ++ "/build_res")
    cachePath <- buildCache
    cache <- safeReadFile cachePath
    let new = foldl (updateCache out)
            cache resFiles
    when (length new > 0) $
        handle handler $ writeFile cachePath new
    return $ Just out

copyHandler :: FilePath -> FilePath -> SomeException -> IO ()
copyHandler a b ex = copyFile a b

badDir :: SomeException -> IO [FilePath]
badDir ex = return []

handler :: SomeException -> IO ()
handler ex = putStrLn $ show ex

-- |Replace old hash with new if file already is in the string,
-- otherwise just add it
updateCache :: String -> String -> FilePath -> String
updateCache _ old "" = old
updateCache _ old "." = old
updateCache _ old ".." = old
updateCache hash old file =
    (concat $ replaceHash <$> splitOn "#" <$> splitOn "\n" old)
        ++ file ++ "#" ++ hash
    where
        replaceHash (pkg:oldHash:_)
            | pkg == file = ""
            | otherwise = pkg ++ "#" ++ oldHash ++ "\n"
        replaceHash _ = ""

cloneRepo :: FilePath -> Build -> IO ()
cloneRepo wdir (Build name _ remote _ _) = do
    catch (callCommand $ "git clone " ++ remote ++ " " ++ wdir ++ "/" ++ name)
        handler
        where
            handler :: SomeException -> IO ()
            handler ex = return ()

-- |Helper function for run given parser and create Meta from it, but replace
-- user and commit data with actual
getMeta :: MetaParser a => a -> Build -> String -> IO (Maybe Meta)
getMeta p b branch = do
    c <- getCommitHash
    builder <- getEffectiveUserName
    m <- parse p
    return $ maybe Nothing
        (\ m -> Just $ Meta $ (buildData m)
            { commit=c
            , hash=c
            , builder=builder
            , branch=branch
            , repoName=(buildName b)
            }) m

-- |Resolve parser type from its name
getParser :: String -> Handle -> IO (Build -> String -> IO (Maybe Meta))
getParser "GBS" f = do
    (fromHandle f :: IO GBSParser) >>= return . getMeta
getParser exec f = do
    p <- (fromHandle f :: IO OwnParser)
    return $ getMeta p{parserExec=exec}

getCommitHash = do
    (_, out, _, pid) <- runInteractiveCommand "git rev-parse HEAD"
    waitForProcess pid
    hGetLine out

prepareRepo brunch =
    catch (callCommand $ "git checkout --force "
        ++ brunch ++ " ; git fetch ; git pull origin " ++ brunch) handler
    where
        handler :: SomeException -> IO ()
        handler e =
            putStrLn "Setting up working branch failed. Build current..."

buildWithLog fname cmd = do
    catch (callCommand $ cmd ++ " | tee " ++ fname)
        handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn "Build failed"

copyDirectory from to = getDirectory' from >>= copyTo_ to
