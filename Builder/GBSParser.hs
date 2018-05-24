{-# LANGUAGE InstanceSigs #-}

module Builder.GBSParser (
    fromFiles
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
        let [_, year,mounth,day,hour,min,_] = splitOneOf " -:" $ last $ splitOn "</B>" $ head $ filter (isInfixOf "Start Time:") $ lines content
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


fromFiles :: (FilePath, FilePath) -> IO GBSParser
fromFiles (fhtml, flog) = do
    html <- readFile fhtml
    log <- readFile flog
    return $ GBSParser html log
