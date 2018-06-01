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
        let status = if total > succ || total <= 0
                     then "FAILED"
                     else "SUCCEED"

        let log = buildLog gbsParser
        let arch = dropWhile isSpace $ last $ splitOn "</B>" $ head $ splitOn "</p>" $ last $  splitOn "Arch" content
        -- Use it later for getting hash of build
        let rpmdir = head $ splitOneOf " \n" $ dropWhile isSpace $ last $ splitOn "generated RPM packages can be found from local repo:\n" log
        return $ Meta arch "unknown" "unknown" time "unknown" "unknown" status (replicate 40 '0')
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
