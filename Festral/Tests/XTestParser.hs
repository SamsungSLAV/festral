-- |Parser of tct-test-ta results to the metafile for CI server.
module Festral.Tests.XTestParser (
    XTestParser (..)
) where

import Festral.Tests.TestParser
import System.IO
import Data.List.Split
import Data.Char
import Data.List

-- |Representation of the output of tests.
data XTestParser = XTestParser {testOut :: String} deriving Show


instance TestParser XTestParser where
    fromFile fname = do
        log <- readFile fname
        return $ XTestParser log

    parse parser = zipWith (\[name,res] i -> TestData "Xtest" i name res res res "0.5") res [1 ..]
        where
        res = map (\x-> [head x, if "OK" `elem` x then "TEST_PASS" else "TEST_FAIL"]) $
            (filter (not . (==) "") <$>) <$> filter (\x-> "OK" `elem` x || "FAILED" `elem` x) $
            splitOn " " <$> splitOn "\n" (testOut parser)

    fromWelesFiles [] = return $ XTestParser ""
    fromWelesFiles files = do
        let x = filter (\(n,c) -> "xtest.log" `isInfixOf` n) files
        return $ getParser x

        where
            getParser ((fname, content):_) = XTestParser content
            getParser _ = XTestParser ""
