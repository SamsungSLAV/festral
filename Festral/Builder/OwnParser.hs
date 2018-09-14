{-# LANGUAGE InstanceSigs #-}

-- |This module is used for running user's scripts as build output parser, so it has some more functions excepting 'MetaParser' interface.
module Festral.Builder.OwnParser (
    OwnParser (..),
    parse,
    fromFile,
    setExec
) where

import Festral.Builder.Meta
import System.Process
import Data.List.Split
import System.IO
import Control.Concurrent

-- |This data contains parts needed by 'OwnParser'
data OwnParser = OwnParser
    { buildLog      :: String -- ^Log file with standard output of the build command
    , parserExec    :: FilePath -- ^Path to the user's parser executable.
    } deriving Show

instance MetaParser OwnParser where
    parse :: OwnParser -> IO Meta
    parse parser = do
        (inp, out, err, _) <- runInteractiveCommand $ parserExec parser
        forkIO $ hPutStr inp $ buildLog parser
        log <- hGetContents out
        let meta = lines log
        let [board, buildType, commit, buildTime, toolchain, builder, status,  hash, repoName, branch, outDir] = map last $ map (splitOn "=") meta
        return (Meta board buildType commit buildTime toolchain builder status hash outDir repoName branch)

    fromFile :: FilePath -> IO OwnParser
    fromFile fname = do
        file <- readFile fname
        return $ OwnParser file "exit"

    fromHandle :: Handle -> IO OwnParser
    fromHandle fname = do
        file <- hGetContents fname
        return $ OwnParser file "exit"

-- |Set own parser executable for the 'OwnParser' object.
-- Executable or script provided by user MUST get standard output of the build command on its standard input after running
-- and MUST write to the standard output parsed meta file (see "Festral.Builder.Builder" for file format).
setExec :: FilePath -> OwnParser -> OwnParser
setExec binary parser = OwnParser (buildLog parser) binary
