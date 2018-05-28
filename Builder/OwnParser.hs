{-# LANGUAGE InstanceSigs #-}

module Builder.OwnParser (
    OwnParser (..),
    parse,
    fromFile,
    setExec
) where

import Builder.Meta
import System.Process
import Data.List.Split
import System.IO

data OwnParser = OwnParser
    { buildLog      :: String
    , parserExec    :: FilePath
    } deriving Show

instance MetaParser OwnParser where
    parse :: OwnParser -> IO Meta
    parse parser = do
        (inp, out, err, _) <- runInteractiveProcess (parserExec parser) [buildLog parser] Nothing Nothing
        log <- hGetContents out
        let meta = lines log
        let [board, buildType, commit, buildTime, toolchain, builder, status,  hash] = map last $ map (splitOn "=") meta
        return (Meta board buildType commit buildTime toolchain builder status hash)

    fromFile :: FilePath -> IO OwnParser
    fromFile fname = do
        file <- readFile fname
        return $ OwnParser file "exit"

setExec :: FilePath -> OwnParser -> OwnParser
setExec binary parser = OwnParser (buildLog parser) binary
