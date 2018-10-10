{-# LANGUAGE RecordWildCards #-}

-- |This module describes metafile used by web page and database for showing and
-- extracting information about builds and how this metafile can be acquired.
module Festral.Builder.Meta (
    Meta(..),
    MetaParser(..),
    toFile,
    show,
    readMeta,
    fromMetaFile,
    isBuild,
    isTest,
    isMeta,
    findField
) where

import System.IO
import Control.Exception
import Data.List.Split
import Data.List

-- |Class which describes Parser - something that can be cunstructed from the
-- file with output of the build by function 'fromFile' and return 'Meta' parsed
-- from this build output.
class MetaParser a where
    parse :: a -> IO Meta
    fromFile :: FilePath -> IO a
    fromHandle :: Handle -> IO a

-- |Representation of the meta.txt file which is used by database and wep page
-- for extracting information about build results.
data Meta = Meta
    {board      :: String
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
    | MetaTest
    {metaData   :: Meta
    ,tester     :: String
    ,testerName :: String
    ,testTime   :: String
    ,testName   :: String
    ,testStatus :: String
    }
    | NotMeta

buildFields =
    [ ("BOARD"          ,board)
    , ("BUILD_TYPE"     ,buildType)
    , ("COMMIT"         ,commit)
    , ("BUILD_TIME"     ,buildTime)
    , ("TOOLCHAIN"      ,toolchain)
    , ("BUILDER"        ,builder)
    , ("BUILD_STATUS"   ,status)
    , ("BUILD_HASH"     ,hash)
    , ("REPO_NAME"      ,repoName)
    , ("BRANCH"         ,branch)
    , ("OUT_DIR"        ,outDir)
    ]

testFields =
    [ ("TESTER"         ,tester)
    , ("TESTER_NAME"    ,testerName)
    , ("TEST_TIME"      ,testTime)
    , ("TEST_NAME"      ,testName)
    , ("TEST_STATUS"    ,testStatus)
    ]

instance Show Meta where
    show m@Meta{..} = unparse m buildFields
    show m@MetaTest{..} = (show $ metaData) ++ (unparse m testFields)
    show _ = "Invalid meta data"

unparse m = foldl (\ s (name, f) -> s ++ name ++ "=" ++ f m ++ "\n") ""

-- |Read Meta from serialized string. This function can works right only on
-- metafiles generated by 'Show Meta' instance because it parse string by order,
-- not by keys!!!
readMeta :: String -> Meta
readMeta str = chooseMeta filledMeta
    where
    filledMeta = readMeta'
        (orderFields (f str) buildFields ++ orderFields (f str) testFields)
    readMeta' (board:bType:commit:bTime:
               toolc:builder:stat:hash:
               repName:branch:outDir:t:
               tName:tTime:testName:testStatus:_) =
        MetaTest (Meta
                    board bType commit
                    bTime toolc builder
                    stat hash outDir
                    repName branch) t tName tTime testName testStatus

    f x = map (splitOn "=") $ filter (isInfixOf "=") $ splitOn "\n" x

chooseMeta m@MetaTest{..}
    | testName == "" || testTime == "" || testStatus == "" = chooseMeta metaData
    | otherwise = m
chooseMeta m@Meta{..}
    | buildType == "" || status == "" || hash == "" || repoName == "" = NotMeta
    | otherwise = m

orderFields str fields = map (\ (x,_) -> findField x str) fields

findField :: String -> [[String]] -> String
findField "" _ = ""
findField name (x:xs)
    | head x == name = last x
    | otherwise = findField name xs
findField name [] = ""

-- |Write 'Meta' to the file at given path
toFile :: Meta -> FilePath -> IO ()
toFile m fname = do
    writeFile fname $ show m

-- |Read 'Meta' from file.
fromMetaFile :: FilePath -> IO Meta
fromMetaFile fname = do
    mdata <- catch (readFile fname) handler
    return $ readMeta mdata

    where
        handler :: SomeException -> IO String
        handler e = return ""

-- |Check if given meta is Meta
isBuild Meta{} = True
isBuild _ = False

-- |Check if given meta is MetaTest
isTest MetaTest{} = True
isTest _ = False

-- |Check if given meta is valid meta type
isMeta x = isBuild x || isTest x
