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

-- |Functions for colorized and formatted writing to console.
module Festral.Internal.Logger (
    putAsyncLog,
    putStrColor,
    putLogColor,
    putLog,
    colorBrace,
    colorBoldBrace,
    module System.Console.ANSI
) where

import Festral.Meta
import System.Console.ANSI
import Control.Concurrent.MVar

-- |Synchronize putting log by given 'MVar'.
putAsyncLog :: MVar a -> IO b -> IO b
putAsyncLog lock f = withMVar lock $ \_ -> f

-- |Put given string with given color .
putStrColor :: Color -> String -> IO ()
putStrColor c s = do
    setSGR [SetColor Foreground Vivid c]
    putStr s
    setSGR [Reset]

-- |Put given strings with given color in squared braces each one, appended
-- to the reposytory name and branch name got from givem 'Meta'. Prepending
-- fields are printed in 'Blue' color like
-- @
-- [repo name][branch name][string1][string2][...]
-- @
putLogColor :: Meta -> Color -> [String] -> IO ()
putLogColor m c y = do
    colorBrace (repoName  $>> m) Blue
    colorBrace (branch $>> m) Blue
    mapM_ (flip colorBoldBrace c) y

-- |The same as 'putLogColor', but without color.
putLog :: Meta -> String -> IO ()
putLog m y = do
    colorBrace (repoName $>> m) Blue
    colorBrace (branch $>> m) Blue
    putStrLn y

-- |Put given string with given color in the square braces.
colorBrace :: String -> Color -> IO ()
colorBrace x color = do
    putStr "["
    putStrColor color x
    putStr "]"

-- |The same as 'colorBrace', but with bold font.
colorBoldBrace :: String -> Color -> IO ()
colorBoldBrace x color = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr "["
    putStrColor color x
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr "]"
    setSGR [Reset]
