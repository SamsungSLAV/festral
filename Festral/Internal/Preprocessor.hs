{-
 - Copyright (c) 2018-2019 Samsung Electronics Co., Ltd All Rights Reserved
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
-- Syntax tree of the test scenario language is below:
--
-- @
-- b    ::= true | false | not b | b opb b | w cmp w
-- opb  ::= && | ||
--
-- w    ::= %text | "text" | @text | [w.]
--
-- cmp  ::= == | !=
--
-- stmt ::= raw (text) | if (b) stmt else stmt fi | include (w)
--          | insert (w) | exec(text) | push(text1, text2)
--          | push_latest(src,dst) | pull(text1)
--          | images(w,w,...) | partition(int, w) | test_header()
--          | boot(login, password) | test()
--          | [stmt;]
--
-- comments: /* */
-- @
--
-- Variable prefixed with '%' char will be replaced with one of 'TestUnit'
-- field.
--
-- Fields of 'TestUnit' understandable by preprocessor are: 'repo', 'parser',
-- 'name', 'target', 'yaml'.
--
-- Variable prefixed with '$' character will be used as string literal.

{-# LANGUAGE FlexibleContexts #-}

module Festral.Internal.Preprocessor (
    preprocess
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import System.Environment
import Control.Exception hiding (try)
import System.FilePath.Posix
import System.IO.Unsafe

import Festral.Tests.Data
import Festral.Internal.Files

data BExpr
    = BVal Bool
    | Not BExpr
    | BinOp BOp BExpr BExpr
    | EqlStr CmpExpr
    deriving Show

data BOp = And | Or
    deriving Show

data CmpExpr
    = Eql PWord PWord
    | NEql PWord PWord
    deriving Show

data Stmt
    = Raw String
    | If BExpr Stmt Stmt
    | Include PWord
    | Insert PWord
    | Exec PWord
    | Push PWord PWord
    | PushLatest PWord PWord
    | Pull PWord
    | Partition PWord PWord
    | Images [PWord]
    | TestHeader
    | Boot PWord PWord
    | Test
    | Seq [Stmt]
    deriving Show

data PWord = VarName String | Str String | Env String | PWSeq [PWord]
    deriving (Show, Eq)

-- Preprocess given string wiht data from given test unit according syntax
-- described in this module description.
preprocess :: TestUnit -> String -> IO String
preprocess _ "" = return ""
preprocess test str = parseSource test str

{-# NOINLINE parseWord #-}
parseWord :: TestUnit -> PWord -> String
parseWord u (VarName x) = u <-| x
parseWord _ (Str x) = x
parseWord _ (Env x) = unsafePerformIO $ handle envDoesNotExists $ getEnv x
    where
        envDoesNotExists :: SomeException -> IO String
        envDoesNotExists _ = return ""
parseWord t (PWSeq (x:xs)) = parseWord t x ++ parseWord t (PWSeq xs)
parseWord _ _ = ""

parseSource :: TestUnit -> String -> IO String
parseSource test x = fmap unlines $ parseSource' test $ parseStr test x

parseSource' :: Show a => TestUnit -> Either a Stmt -> IO [String]
parseSource' t (Right x) = filter (not . null) <$> lines <$> parseStatement t x
parseSource' _ (Left x) = print x >> return []

parseStr :: TestUnit -> String -> Either ParseError Stmt
parseStr t x = parse (statement t) "" x

parseStatement :: TestUnit -> Stmt -> IO String
parseStatement _ (Raw x) = return x
parseStatement t (If c a1 a2) = parseStatement t
    $ if (boolExpr c) then a1 else a2
parseStatement t (Include x) = do
    let fname = parseWord t x
    file <- safeReadFile fname
    if (takeExtension fname == ".ftc")
        then  preprocess t file
        else  return file
parseStatement t (Insert x) = return $ parseWord t x
parseStatement t (Exec x) = return $
    "              ##TEMPLATE_RUN " ++ parseWord t x ++ "##\n"
parseStatement t (Push src dest) = return $
    pushHelper "TEMPLATE_URL" t src parsedDest
    where parsedDest = parseWord t dest
parseStatement t (PushLatest src dest) = return $
    pushHelper "TEMPLATE_LATEST" t src parsedDest
    where parsedDest = parseWord t dest
parseStatement t (Pull src) = return $
    "              - pull:\n\
    \                  src: '" ++ parsedSrc ++ "'\n\
    \                  alias: '"++ takeFileName parsedSrc ++ "'\n"
    where parsedSrc = parseWord t src
parseStatement t (Images lst) = return $
    "  - deploy:\n\
    \      images:\n"
    ++ (uris lst)
    where
        uris [] = "      partition_layout:\n"
        uris (x:xs) =
            "        - " ++ parseWord t x ++ "\n\
            \          compression: 'zip'\n"
            ++ uris xs
parseStatement t (Partition n f) = return $
    "        - id: " ++ parseWord t n ++ "\n\
    \          image_name: " ++ parseWord t f ++ "\n"
parseStatement t TestHeader = return $
    "device_type: " ++ parseWord t (VarName "target") ++ "\n\
    \job_name: " ++ testCaseName t ++ "\n\
    \priority: medium\n\
    \actions:\n"
parseStatement t (Boot login password) = return $
    "  - boot:\n\
    \      login: " ++ parseWord t login ++ "\n\
    \      password: " ++ parseWord t password ++ "\n"
parseStatement t Test = return $
    "  - test:\n\
    \      name: " ++ testCaseName t ++ "\n\
    \      test_cases:\n\
    \        - case_name: " ++ parseWord t (VarName "name") ++ "\n\
    \          test_actions:\n"
parseStatement t (Seq (x:xs)) = do
    a <- parseStatement t x
    b <- parseStatement t (Seq xs)
    return $ unlines [a, b]
parseStatement _ _ = return ""

pushHelper x t src dst =
    "              - push:\n\
    \                  ##" ++ x ++ " " ++ parseWord t src ++ "##\n\
    \                  dest: '" ++ dst ++ "'\n\
    \                  alias: '"++ takeFileName dst ++ "'\n"

testCaseName t = parseWord t (VarName "name") ++ "-"
    ++ parseWord t (VarName "target")

boolExpr :: BExpr -> Bool
boolExpr (BVal x) = x
boolExpr (Not x) = not $ boolExpr x
boolExpr (BinOp And a1 a2) = (boolExpr a1) && (boolExpr a2)
boolExpr (BinOp Or a1 a2) = (boolExpr a1) || (boolExpr a2)

cmpExpr :: TestUnit -> CmpExpr -> BExpr
cmpExpr t (Eql a b) = if parseWord t a == parseWord t b
                        then BVal True
                        else BVal False
cmpExpr t (NEql a b) = Not $ cmpExpr t (Eql a b)

def = emptyDef
    { identStart    = letter
    , identLetter   = alphaNum
    , opStart       = oneOf "&|=!%$@"
    , reservedOpNames = ["&&", "||", "==", "!=", "!", "%", "$", "@"]
    , reservedNames = [ "true", "false", "raw", "if", "fi", "else"
                      , "include", "insert", "exec", "push", "pull"
                      , "push_latest", "images", "partition", "test_header"
                      , "boot", "test"
                      ]
    }

tokenParser = makeTokenParser def
resOp = reservedOp tokenParser

varStmt :: Parser PWord
varStmt = char '%' >> VarName <$> many1 alphaNum

strStmt :: Parser PWord
strStmt = between (char '"') (char '"') $ inQuotes Str

inQuotes :: (String -> PWord) -> Parser PWord
inQuotes f = do
    x <- manyTill anyChar (lookAhead $ string "\"")
    return $ f x

envStmt :: Parser PWord
envStmt = char '@' >> Env <$> many1 alphaNum

pwordSeq :: Parser PWord
pwordSeq = do
    list <- (sepBy1 pwordStmt $ dot tokenParser)
    return $ if length list == 1 then head list else PWSeq list

pwordStmt = varStmt <|> strStmt <|> envStmt

gEqExpr a b = do
    x <- pwordSeq
    spaces
    reserved tokenParser a
    y <- pwordSeq
    spaces
    return $ b x y

eqExpr :: Parser CmpExpr
eqExpr = gEqExpr "==" Eql

neqExpr :: Parser CmpExpr
neqExpr = gEqExpr "!=" NEql

cmpStmt :: Parser CmpExpr
cmpStmt = eqExpr <|> neqExpr

bExpr :: TestUnit -> Parser BExpr
bExpr t = buildExpressionParser bOperators (bTerm t)

bOperators = [[Prefix (resOp "!"  >> return (Not))]
             ,[Infix  (resOp "&&" >> return (BinOp And )) AssocLeft
              ,Infix  (resOp "||" >> return (BinOp Or  )) AssocLeft
             ]]

bTerm t = (parens tokenParser) (bExpr t)
    <|> (resOp "true"  >> return (BVal True))
    <|> (resOp "false" >> return (BVal False))
    <|> (cmpExpr t <$> cmpStmt)

statementList :: TestUnit -> Parser Stmt
statementList t = do
    list <- (endBy1 (statement' t) $ semi tokenParser)
    return $ if length list == 1 then head list else Seq list

statement :: TestUnit -> Parser Stmt
statement t = statementList t

statement' :: TestUnit -> Parser Stmt
statement' t
    = (ifStmt t)
    <|> includeStmt
    <|> insertStmt
    <|> rawStmt
    <|> execStmt
    <|> pushLatestStmt
    <|> pushStmt
    <|> pullStmt
    <|> imagesStmt
    <|> partitionStmt
    <|> testHeaderStmt
    <|> bootStmt
    <|> testStmt

genStmt a b = do
    reserved tokenParser a
    between (char '(') (char ')') $ inParens b

rawStmt :: Parser Stmt
rawStmt = genStmt "raw" Raw

execStmt :: Parser Stmt
execStmt = genPword "exec" Exec

pushStmt :: Parser Stmt
pushStmt = genPword2Args "push" Push

pushLatestStmt :: Parser Stmt
pushLatestStmt = genPword2Args "push_latest" PushLatest

pullStmt :: Parser Stmt
pullStmt = genPword "pull" Pull

partitionStmt :: Parser Stmt
partitionStmt = genPword2Args "partition" Partition

testHeaderStmt :: Parser Stmt
testHeaderStmt = genEmpty "test_header" TestHeader

bootStmt :: Parser Stmt
bootStmt = genPword2Args "boot" Boot

testStmt :: Parser Stmt
testStmt = genEmpty "test" Test

imagesStmt :: Parser Stmt
imagesStmt = do
    reserved tokenParser "images"
    spacyChar '('
    x <- (sepBy1 pwordStmt $ spacyChar ',')
    spacyChar ')'
    return $ Images x

spacyChar x
    =  spaces >> optional newline >> spaces
    >> char x
    >> spaces >> optional newline >> spaces

genEmpty n y = do
    reserved tokenParser n
    spacyChar '('
    spacyChar ')'
    return y

genPword n y = do
    reserved tokenParser n
    spacyChar '('
    x <- pwordSeq
    spacyChar ')'
    return $ y x

genPword2Args n y = do
    reserved tokenParser n
    spacyChar '('
    x1 <- pwordSeq
    spacyChar ','
    x2 <- pwordSeq
    spacyChar ')'
    return $ y x1 x2

includeStmt :: Parser Stmt
includeStmt = genPword "include" Include

insertStmt :: Parser Stmt
insertStmt = genPword "insert" Insert

ifStmt :: TestUnit -> Parser Stmt
ifStmt t = do
    reserved tokenParser "if"
    cond <- bExpr t
    stmt1 <- statement t
    spaces
    reserved tokenParser "else"
    stmt2 <- statement t
    spaces
    reserved tokenParser "fi"
    return $ If cond stmt1 stmt2

inParens :: (String -> Stmt) -> Parser Stmt
inParens f = do
    x <- manyTill anyChar (lookAhead $ string ")")
    return $ f x

x <-| "repo"        = repo $ tConfig x
x <-| "parser"      = parser $ tConfig x
x <-| "name"        = name $ tConfig x
x <-| "target"      = target x
x <-| "yaml"        = yaml $ tConfig x
x <-| "testOutFile" = testOutFile $ tConfig x
x <-| s             = s
