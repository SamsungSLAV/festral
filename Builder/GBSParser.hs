{-# LANGUAGE InstanceSigs #-}

module Builder.GBSParser (
    GBSParser (..),
    parse,
    fromFile
) where

import Builder.Meta
import Data.List.Split
import Data.List

data GBSParser = GBSParser {
    buildHtml    :: String
    ,buildLog    :: String
} deriving Show

instance MetaParser GBSParser where

    parse :: GBSParser -> IO Meta
    parse gbsParser = do
        let content = buildHtml gbsParser
        let [_, year,mounth,day,hour,min,_] = splitOneOf " :-" $ head $ splitOn "\n" $ last $ splitOn "Start Time:" content
        let time = year ++ mounth ++ day ++ hour ++ min ++ "00"

        let stat = map (takeWhile ((/= '<'))) $ map (!!1) $ map (splitOn "<td>") $ take 2 $ filter (isInfixOf "<td>") $ lines content
        let [total, succ] = (map read stat) :: [Int]
        let status = if total > succ
            then "FAILED"
            else "SUSSEED"

        let log = buildLog gbsParser
        let hash = head $ filter ((/=)"") $ splitOneOf "' " $ last $ splitOn "from" $ head $ filter (isInfixOf "Creating (native) source archive") $ lines log

        let arch = takeWhile ((/=)'<') $ last $ splitOn "</B> " $ head $ filter (isInfixOf "Arch") $ lines content
        return $ Meta arch "" "" time "" "" status hash


    fromFile :: FilePath -> IO GBSParser
    fromFile flog = do
        log <- readFile flog
        let fhtml = last $ splitOneOf " " $ head $ lines $ last $ splitOn "generated html format report:\n" log
        html <- readFile fhtml
        return $ GBSParser html log
