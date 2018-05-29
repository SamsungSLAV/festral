{-# LANGUAGE InstanceSigs #-}

module Builder.GBSParser (
    GBSParser (..),
    parse,
    fromFile
) where

import Builder.Meta
import Data.List.Split
import Data.List
import Control.Exception
import Data.Char
import Data.Time

data GBSParser = GBSParser {
    buildHtml    :: String
    ,buildLog    :: String
} deriving Show

instance MetaParser GBSParser where

    parse :: GBSParser -> IO Meta
    parse gbsParser = do
        let content = buildHtml gbsParser
        time <- show <$> getZonedTime
        let (year:mounth:day:hour:min:secs:_) = splitOneOf " :-." time
        let time = year ++ mounth ++ day ++ hour ++ min ++ secs

        let stat = map (takeWhile ((/= '<'))) $ map (!!1) $ map (splitOn "<td>") $ take 2 $ filter (isInfixOf "<td>") $ lines content
        let [total, succ] = parseStat $ (map read stat) :: [Int]
        let status = if total > succ
            then "FAILED"
            else "SUSSEED"

        let log = buildLog gbsParser
        let (_,hash') = partition (`elem` [""]) $  splitOneOf " '" $ head $ reverse $ splitOn "from" $ head $ splitOn "\n" $ last $ splitOn "Creating (native) source archive" log
        let hash = if hash' == []
            then "0000000000000000000000000000000000000000"
            else head hash'

        let arch = dropWhile isSpace $ last $ splitOn "</B>" $ head $ splitOn "</p>" $ last $  splitOn "Arch" content
        return $ Meta arch "" "" time "" "" status hash
        where
            parseStat x@[_,_] = x
            parseStat _ = [0, -1]

    fromFile :: FilePath -> IO GBSParser
    fromFile flog = catch (fromFile' flog) handler
        where
            handler :: SomeException -> IO GBSParser
            handler e = return (GBSParser "" "")

fromFile' :: FilePath -> IO GBSParser
fromFile' flog = do
    log <- readFile flog
    let fhtml = last $ splitOneOf " " $ head $ lines $ last $ splitOn "generated html format report:\n" log
    html <- readFile fhtml
    return $ GBSParser html log
