module Builder.Meta (
    Meta(..),
    MetaParser(..),
    tofile
) where

class MetaParser a where
    parse :: a -> IO Meta

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
tofile :: Meta -> FilePath -> IO ()
tofile m fname = do
    let content = ("BOARD=" ++ board m
                    ++ "\nBUILD_TYPE=" ++ buildType m
                    ++ "\nCOMMIT="     ++ commit m
                    ++ "\nBUILD_TIME=" ++ buildTime m
                    ++ "\nTOOLCHAIN="  ++ toolchain m
                    ++ "\nBUILDER="    ++ builder m
                    ++ "\nBUILD_STATUS=" ++ status m
                    ++ "\nBUILD_HASH=" ++ hash m ++ "\n")
    writeFile fname content

