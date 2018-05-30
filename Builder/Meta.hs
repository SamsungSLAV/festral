-- |This module describes metafile used by web page and database for showing and extracting information
-- about builds and how this metafile can be acquired.
module Builder.Meta (
    Meta(..),
    MetaParser(..),
    toFile
) where

-- |Class which describes Parser - something that can be cunstructed from the file
-- with output of the build by function 'fromFile' and return 'Meta' parsed from this build output.
class MetaParser a where
    parse :: a -> IO Meta
    fromFile :: FilePath -> IO a

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
} deriving Show

-- |Write 'Meta' to the file at given path
toFile :: Meta -> FilePath -> IO ()
toFile m fname = do
    let content = ("BOARD=" ++ board m
                    ++ "\nBUILD_TYPE=" ++ buildType m
                    ++ "\nCOMMIT="     ++ commit m
                    ++ "\nBUILD_TIME=" ++ buildTime m
                    ++ "\nTOOLCHAIN="  ++ toolchain m
                    ++ "\nBUILDER="    ++ builder m
                    ++ "\nBUILD_STATUS=" ++ status m
                    ++ "\nBUILD_HASH=" ++ hash m ++ "\n")
    writeFile fname content

