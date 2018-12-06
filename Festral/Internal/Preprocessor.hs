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

-- |Module gives functions for file preprocessing with syntax as below:
--
-- @
--   IF_EQ_INSERT(config_field) (value) (string)
-- @
-- This command check if __config_field__ equals for __value__, and if yes,
-- insert into this place __string__. The first argument can be name of the
-- 'TestUnit' field.
--
-- @
--   IF_EQ_INSERT_ELSE(config_field) (value) (if true value) (else value)
-- @
-- The same as IF_EQ_INSERT, but if first two arguments are not equal, insert
-- __else value__ into this place. The first argument can be name of the
-- 'TestUnit' field.
--
-- @
--   INCLUDE(filepath)
-- @
-- Insert into this place preprocessed and untemplated contents of the given
-- filepath.
--
-- @
--   INSERT(TestUnit field)
-- @
-- Insert into this place content of the field of the 'TestUnit' with given name
-- or if there is no such field, just insert argument as string.
--
-- Fields of 'TestUnit' understandable by preprocessor are: 'repo', 'parser',
-- 'name', 'target', 'yaml'.
module Festral.Internal.Preprocessor (
    preprocess
) where

import Data.List.Split
import Data.Char

import Festral.Tests.Data

-- |Type represents preprocessor commands tree
data PreprocessUnit
    =
    -- | IF_EQ_INSERT(config_field) (value) (string)
    PreprocessIf
        { ifField       :: String
        , ifValue       :: String
        , ifInsert      :: String
        }
    -- | IF_EQ_INSERT_ELSE(config_field) (value) (if true value) (else value)
    | PreprocessIfElse
        { ifField       :: String
        , ifValue       :: String
        , ifInsert      :: String
        , elseInsert    :: String
        }
    -- | INCLUDE(filepath)
    | PreprocessInclude String
    -- | INSERT(TestUnit_field)
    | PreprocessInsert  [String]
    | NotPreprocess String
    deriving Show

extractArgs x = filter (not . isBlank) $
                split (dropDelims . dropBlanks $ oneOf  "()") x

isBlank :: String -> Bool
isBlank = all isSpace

processRow :: String -> PreprocessUnit
processRow = processRow' . extractArgs

processRow' :: [String] -> PreprocessUnit
processRow' ("IF_EQ_INSERT_ELSE":f:v:i:e:_) = PreprocessIfElse f v i e
processRow' ("IF_EQ_INSERT":f:v:i:_) = PreprocessIf f v i
processRow' ("INCLUDE":f:_) = PreprocessInclude f
processRow' ("INSERT":xs) = PreprocessInsert xs
processRow' x = NotPreprocess $ concat x

preprocess' :: TestUnit -> PreprocessUnit -> String
preprocess' t (PreprocessIf f v i) = preprocess' t (PreprocessIfElse f v i "")
preprocess' t (PreprocessIfElse f v i e) = if (t <-| f) == v then i else e
preprocess' t (PreprocessInclude x) = "##TEMPLATE_FILE " ++ x ++ "##"
preprocess' t (PreprocessInsert x) = concat $ map ((<-|) t) x
preprocess' _ (NotPreprocess x) = x

-- |Preprocess given string wiht data from given test unit according syntax
-- described in "Preprocessor".
preprocess :: TestUnit -> String -> String
preprocess test str = unlines $ preprocess' test <$> processRow <$> lines str

x <-| "repo"    = repo $ tConfig x
x <-| "parser"  = parser $ tConfig x
x <-| "name"    = name $ tConfig x
x <-| "target"  = target x
x <-| "yaml"    = yaml $ tConfig x
x <-| s         = s
