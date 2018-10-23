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

{-# LANGUAGE InstanceSigs #-}

-- |This module is used for running user's scripts as build output parser, so
-- it has some more functions excepting 'MetaParser' interface.
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
        return $ readMeta log

    fromFile :: FilePath -> IO OwnParser
    fromFile fname = do
        file <- readFile fname
        return $ OwnParser file "exit"

    fromHandle :: Handle -> IO OwnParser
    fromHandle fname = do
        file <- hGetContents fname
        return $ OwnParser file "exit"

-- |Set own parser executable for the 'OwnParser' object.
-- Executable or script provided by user MUST get standard output of the build
-- command on its standard input after running
-- and MUST write to the standard output parsed meta file (see
-- "Festral.Builder.Builder" for file format).
setExec :: FilePath -> OwnParser -> OwnParser
setExec binary parser = OwnParser (buildLog parser) binary
