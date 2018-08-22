-- |This module describes metafile used by web page and database for showing and extracting information
-- about builds and how this metafile can be acquired.
module Festral.Builder.Meta (
    Meta(..),
    MetaParser(..),
    toFile,
    show,
    readMeta,
    fromMetaFile
) where

import System.IO
import Control.Exception
import Data.List.Split
import Data.List

-- |Class which describes Parser - something that can be cunstructed from the file
-- with output of the build by function 'fromFile' and return 'Meta' parsed from this build output.
class MetaParser a where
    parse :: a -> IO Meta
    fromFile :: FilePath -> IO a
    fromHandle :: Handle -> IO a

-- |Representation of the meta.txt file which is used by database and wep page for
-- extracting information about build results.
data Meta = Meta {
    board       :: String
    ,buildType  :: String
    ,commit     :: String
    ,buildTime  :: String
    ,toolchain  :: String
    ,builder    :: String
    ,status     :: String
    ,hash       :: String
    ,outDir     :: FilePath
    ,repoName   :: String
    ,branch     :: String
    }
    | MetaTest {
    metaData    :: Meta
    ,tester     :: String
    ,testerName :: String
    ,testTime   :: String
    ,testName   :: String
    }

instance Show Meta where
    show (Meta board bType commit bTime toolc builder stat hash outDir repName branch) = "BOARD=" ++ board
                                                    ++ "\nBUILD_TYPE=" ++ bType
                                                    ++ "\nCOMMIT=" ++ commit
                                                    ++ "\nBUILD_TIME=" ++ bTime
                                                    ++ "\nTOOLCHAIN=" ++ toolc
                                                    ++ "\nBUILDER=" ++ builder
                                                    ++ "\nBUILD_STATUS=" ++ stat
                                                    ++ "\nBUILD_HASH=" ++ hash
                                                    ++ "\nREPO_NAME=" ++ repName
                                                    ++ "\nBRANCH=" ++ branch
                                                    ++ "\nOUT_DIR=" ++ outDir
    show (MetaTest m t tName tTime name) = (show m)
                                        ++ "\nTESTER=" ++ t
                                        ++ "\nTESTER_NAME=" ++ tName
                                        ++ "\nTEST_TIME=" ++ tTime
                                        ++ "\nTEST_NAME=" ++ name

-- |Read Meta from serialized string. This function can works right only on metafiles generated by 'Show Meta' instance
-- because it parse string by order, not by keys!!!
-- TODO: parse Meta really by keys.
readMeta :: String -> Meta
readMeta str = readMeta' (f str)
    where
    readMeta' (board:bType:commit:bTime:toolc:builder:stat:hash:repName:branch:outDir:t:tName:tTime:name:_) =
        MetaTest (Meta board bType commit bTime toolc builder stat hash outDir repName branch) t tName tTime name
    readMeta' (board:bType:commit:bTime:toolc:builder:stat:hash:repName:branch:outDir:t:tName:tTime:_) =
        MetaTest (Meta board bType commit bTime toolc builder stat hash outDir repName branch) t tName tTime "unknown"
    readMeta' (board:bType:commit:bTime:toolc:builder:stat:hash:repName:branch:outDir:_) = Meta board bType commit bTime toolc builder stat hash outDir repName branch
    f x = map last $ map (splitOn "=") $ filter (isInfixOf "=") $ splitOn "\n" x


-- |Write 'Meta' to the file at given path
toFile :: Meta -> FilePath -> IO ()
toFile m fname = do
    writeFile fname $ show m

-- |Read 'Meta' from file. Now it can read only files created by 'toFile' function.
fromMetaFile :: FilePath -> IO Meta
fromMetaFile fname = do
    mdata <- catch (readFile fname) handler
    return $ readMeta mdata

    where
        handler :: SomeException -> IO String
        handler e = putStrLn (show e) >> return ""

