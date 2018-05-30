module Main (
    main
) where

import System.Environment
import Builder.Builder

main = do
    args <- getArgs
    let res = if (length args) /= 3
                then putStrLn "Usage: festral-build <config json> <repositoy location> <output directory>"
                else do
                    let [cfg, repodir, outdir] = args
                    Just builder <- builderFromFile cfg
                    mapM_ (\x -> build x repodir outdir) builder
    res
                                                                                                        
