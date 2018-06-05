{-# LANGUAGE DeriveGeneric #-}

module Festral.Tests.Test (
    runTest,
    TestRunnerConfig (..)
) where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as LB
import Festral.Weles.API
import Data.Maybe

data TestConfig = TestConfig
    { repo  :: String
    , yaml  :: FilePath
    } deriving (Show, Generic)

instance FromJSON TestConfig
instance ToJSON TestConfig

data TestRunnerConfig = TestRunnerConfig
    { projectsDir   :: FilePath
    , welesIP       :: String
    , welesPort     :: String
    , welesFilePort :: String
    , yamls         :: [TestConfig]
    } deriving (Show, Generic)

instance FromJSON TestRunnerConfig
instance ToJSON TestRunnerConfig

-- |runTest path_to_config_file build_name output_dir
runTest :: FilePath -> String -> FilePath -> IO ()
runTest fname target outDir = do
    configStr <- LB.readFile fname
    let config = decode configStr :: Maybe TestRunnerConfig
    let ret = if isNothing config
                then return ()
                else continue $ fromJust config
    ret
    where
        continue config = do
            let targetPath = projectsDir config ++ "/" ++ target
            let yamlPath = yaml $ head $ filter (\x -> repo x == target) $ yamls config
            jobId <- startJob yamlPath
            let jobId' = if isNothing jobId
                        then return (-1)
                        else return $ fromJust jobId
            jobId'' <- jobId'
            job <- getJobWhenDone jobId''
            jobFiles <- getFileList jobId''
            let jobFiles' = if isNothing jobFiles
                                then []
                                else fromJust jobFiles
            mapM_ (\fname -> do
                    content <- getJobOutFile jobId'' fname
                    let content' = if isNothing content 
                                    then ""
                                    else fromJust content
                    writeFile (outDir++ "/" ++ fname) content'
                    ) jobFiles'
