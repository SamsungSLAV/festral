module Main (
    main
) where

import System.Environment
import Festral.Builder.Builder
import Data.Maybe
import System.Directory
import System.IO
import Festral.Files

main = do
    args <- getArgs

    freshBuildsFile <- freshBuilds
    writeFile freshBuildsFile ""

    let res = if (length args) /= 3
                then putStrLn "Usage: festral-build <config json> <repositoy location> <output directory>"
                else do
                    let [cfg, repodir, outdir] = args
                    builder' <- builderFromFile cfg
                    if isJust builder'
                        then do
                            let Just builder = builder'
                            mapM_ (\x -> build x repodir outdir) builder
                        else 
                            putStrLn "Cant get builder from config file"
    res
                                                                                                        
