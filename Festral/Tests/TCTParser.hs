{-# LANGUAGE InstanceSigs #-}

-- |Parser of tct-test-ta results to the metafile for CI server.
module Festral.Tests.TCTParser (
    TCTParser (..)
) where

import Festral.Tests.TestParser
import System.IO
import Data.List.Split
import Data.Char
import Data.List

-- |Representation of the output of tests.
data TCTParser = TCTParser {tctOut :: String} deriving Show


instance TestParser TCTParser where
    fromFile :: FilePath -> IO TCTParser
    fromFile fname = do
        log <- readFile fname
        return $ TCTParser log

    parse :: TCTParser -> [TestData]
    parse parser = zipWith (\[name,res] i -> TestData "TCT_TA_test" i name (tr res) (tr res) (tr res) "0.5") res [1 ..]
        where
        res = map (dropWhile isSpace) <$> (filter ((== 2) . length) $ splitOn "..." <$> splitOn "\n" (tctOut parser))
        tr "OK" = "TEST_PASS"
        tr _ = "TEST_FAIL"

    fromWelesFiles :: [(String, String)] -> IO TCTParser
    fromWelesFiles [] = return $ TCTParser ""
    fromWelesFiles files = do
        let x = filter (\(n,c) -> "tct-test-ta.log" `isInfixOf` n) files
        return $ getParser x

        where
            getParser ((fname, content):_) = TCTParser content
            getParser _ = TCTParser ""
