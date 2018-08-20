{-# LANGUAGE DeriveGeneric #-}

-- |Module for building process managament. It get information about build targets
-- from given .json file formatted as follow:
--
-- @
-- [{"buildName": "name of the project to build",
--   "buildCmd" : "command used to build, e.g. gbs build -A armv7l ...",
--   "buildRepo": "origin repusitory address of the project",
--   "buildResParser" : "name of the built-in parsers ("GBS") or path to the own binary parser, see below",
--   "branch":["master", "other branch", "etc"]
--   }, another build targets ... ]
-- @
--
-- Parser is some script or binary which generates meta.txt file from output of your 'buildCmd' command.
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
--  BUILD_STATUS=result of build (SUCCEED and FAILED are known, but may be there are other ones)
--  BUILD_HASH=hash of the build
-- @
--
-- Parser script must gets output of the 'buildCmd' from its 'stdin' and writes meta file to the 'stdout'
module Festral.Builder.Builder (
    builderFromFile,
    build
) where

import Festral.Builder.Meta
import Festral.Builder.OwnParser
import Festral.Builder.GBSParser
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as LB
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
import Festral.Files

-- |Build structure which represents config json format.
data Build = Build
    { buildName         :: String -- ^ Name of the project to be built, it must be name of the root directory containing project
    , buildCmd          :: String -- ^ Command used for building
    , buildRepo         :: String -- ^ Remote adress of the repository, used for first cloning project
    , buildResParser    :: String -- ^ Parser of the 'buildCmd' output, it can be "GBS" for using standard GBS parser or name of uour own binary
    , branches          :: [String] -- ^ List of branch names to be built
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

    fromHandle h = do
        p <- fromHandle h
        return $ GBS p

-- |Gets builder object from given configuration json.
builderFromFile :: FilePath -> IO (Maybe [Build])
builderFromFile fname = do
    file <- LB.readFile fname
    return $ decode file

-- |Build target located in the first path + build name and put meta file to the directory tree with given in seconf path root directory
-- build buildObject rood_dir_of_project root_dir_of_output_files
build :: Build -> FilePath -> FilePath -> IO ()
build build wdir outdir = do
    cloneRepo wdir build
    let srcDir = wdir ++ "/" ++ buildName build
    mapM_ (\x -> handle handler $ withCurrentDirectory srcDir (buildOne srcDir x)) (branches build)
    where
        buildOne srcDir branch = do
            (logfile, loghandle) <- openTempFileWithDefaultPermissions "/tmp" "build.log"
            prepareRepo srcDir branch
            buildWithLog logfile (buildCmd build) srcDir
            parser <- getParser (buildResParser build) loghandle
            meta <- getMeta parser build branch
            let outDirName = outdir ++ "/" ++ hash meta ++ "_" ++ buildTime meta
            createDirectoryIfMissing True outDirName
            toFile meta (outDirName ++ "/meta.txt")
            catch (renameDirectory (outDir meta) (outDirName ++ "/build_res")) handler
            catch (renameFile logfile (outDirName ++ "/build.log")) (copyHandler logfile (outDirName ++ "/build.log"))
            bLogFile <- freshBuilds
            appendFile bLogFile (hash meta ++ "_" ++ buildTime meta ++ "\n")

            resFiles <- handle badDir $ getDirectoryContents (outDirName ++ "/build_res")
            cachePath <- buildCache
            cache <- handle badFile $ readFile cachePath
            let new = foldl (updateCache (hash meta ++ "_" ++ buildTime meta)) cache resFiles
            when (length new > 0) $
                handle handler $ writeFile cachePath new
                where
                    copyHandler :: FilePath -> FilePath -> SomeException -> IO ()
                    copyHandler a b ex = copyFile a b
                    badDir :: SomeException -> IO [FilePath]
                    badDir ex = putStrLn (show ex) >> return [""]
                    badFile :: SomeException -> IO String
                    badFile ex = putStrLn (show ex) >> return ""

        handler :: SomeException -> IO ()
        handler ex = putStrLn $ show ex

updateCache :: String -> String -> FilePath -> String
updateCache _ old "." = old
updateCache _ old ".." = old
updateCache hash old file = (concat $ replaceHash <$> splitOn "#" <$> splitOn "\n" old) ++ file ++ "#" ++ hash
    where
        replaceHash (pkg:oldHash:_)
            | pkg == file = ""
            | otherwise = pkg ++ "#" ++ oldHash ++ "\n"
        replaceHash _ = ""

cloneRepo :: FilePath -> Build -> IO ()
cloneRepo wdir (Build name _ remote _ _) = do
    catch (callCommand $ "git clone " ++ remote ++ " " ++ wdir ++ "/" ++ name) handler
        where
            handler :: SomeException -> IO ()
            handler ex = return ()

getMeta :: Parser a -> Build -> String -> IO Meta
getMeta p b branch = do
    commit <- getCommitHash
    builder <- getEffectiveUserName
    m <- parse p
    let m' = Meta (board m) (buildType m) commit (buildTime m) (toolchain m) builder (status m) commit (outDir m) (buildName b) branch
    return m'

getParser :: String -> Handle -> IO (Parser a)
getParser "GBS" f = do
    p <- fromHandle f
    return $ GBS p
getParser exec f = do
    p <- fromHandle f
    let p' = setExec exec p
    return $ Own p'

getCommitHash = do
    (_, out, _, pid) <- runInteractiveCommand "git rev-parse HEAD"
    waitForProcess pid
    hGetLine out

prepareRepo srcDir brunch =
    catch (callCommand $ "git checkout --force "
        ++ brunch ++ " ; git fetch ; git pull origin " ++ brunch) handler
    where
        handler :: SomeException -> IO ()
        handler e = putStrLn "Setting up working branch failed. Build current..."

buildWithLog fname cmd wdir = do
    catch (callCommand $ "cd " ++ wdir ++ " ; " ++ cmd ++ " | tee " ++ fname) handler
    where
        handler :: SomeException -> IO ()
        handler ex = putStrLn "Build failed"
