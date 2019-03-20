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
-- w    ::= %text | $text
--
-- cmp  ::= == | !=
--
-- stmt ::= raw (text) | if (b) expr else expr fi | include (text)
--          | insert (w) | [stmt;]
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
module Festral.Internal.Preprocessor (
    preprocess
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

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
    | Include FilePath
    | Insert PWord
    | Seq [Stmt]
    deriving Show

data PWord = VarName String | Str String
    deriving (Show, Eq)

-- Preprocess given string wiht data from given test unit according syntax
-- described in this module description.
preprocess :: TestUnit -> String -> IO String
preprocess test str = parseSource test str

parseWord :: TestUnit -> PWord -> String
parseWord u (VarName x) = u <-| x
parseWord _ (Str x) = x

parseSource :: TestUnit -> String -> IO String
parseSource test x = fmap unlines $ parseSource' test $ parseStr x

parseSource' :: Show a => TestUnit -> Either a Stmt -> IO [String]
parseSource' t (Right x) = lines <$> parseStatement t x
parseSource' _ (Left x) = print x >> return []

parseStr :: String -> Either ParseError Stmt
parseStr x = parse statementList "" x

parseStatement :: TestUnit -> Stmt -> IO String
parseStatement _ (Raw x) = return x
parseStatement t (If c a1 a2) = parseStatement t
    $ if (boolExpr c) then a1 else a2
parseStatement _ (Include x) = safeReadFile x
parseStatement t (Insert x) = return $ parseWord t x
parseStatement t (Seq (x:xs)) = do
    a <- parseStatement t x
    b <- parseStatement t (Seq xs)
    return $ unlines [a, b]
parseStatement _ _ = return ""

boolExpr :: BExpr -> Bool
boolExpr (BVal x) = x
boolExpr (Not x) = not $ boolExpr x
boolExpr (BinOp And a1 a2) = (boolExpr a1) && (boolExpr a2)
boolExpr (BinOp Or a1 a2) = (boolExpr a1) || (boolExpr a2)

cmpExpr :: CmpExpr -> BExpr
cmpExpr (Eql a b) = if a == b then BVal True else BVal False
cmpExpr (NEql a b) = if a /= b then BVal True else BVal False

def = emptyDef
    { commentStart  = "/*"
    , commentEnd    = "*/"
    , commentLine   = "//"
    , identStart    = letter
    , identLetter   = alphaNum
    , opStart       = oneOf "&|=!%$"
    , reservedOpNames = ["&&", "||", "==", "!=", "!", "%", "$"]
    , reservedNames = [ "true", "false", "raw", "if", "else"
                      , "include", "insert"
                      ]
    }

tokenParser = makeTokenParser def
resOp = reservedOp tokenParser

varStmt :: Parser PWord
varStmt = char '%' >> VarName <$> many1 alphaNum

strStmt :: Parser PWord
strStmt = char '$' >> Str <$> many1 alphaNum

pwordStmt = varStmt <|> strStmt

gEqExpr a b = do
    x <- pwordStmt
    spaces
    reserved tokenParser a
    y <- pwordStmt
    spaces
    return $ b x y

eqExpr :: Parser CmpExpr
eqExpr = try $ gEqExpr "==" Eql

neqExpr :: Parser CmpExpr
neqExpr = try $ gEqExpr "!=" NEql

cmpStmt :: Parser CmpExpr
cmpStmt = eqExpr <|> neqExpr

bExpr :: Parser BExpr
bExpr = buildExpressionParser bOperators bTerm

bOperators = [[Prefix (resOp "!"  >> return (Not))]
             ,[Infix  (resOp "&&" >> return (BinOp And )) AssocLeft
              ,Infix  (resOp "||" >> return (BinOp Or  )) AssocLeft
             ]]

bTerm = (parens tokenParser) bExpr
    <|> (resOp "true"  >> return (BVal True))
    <|> (resOp "false" >> return (BVal False))
    <|> (cmpExpr <$> cmpStmt)

statementList :: Parser Stmt
statementList = do
    list <- (sepBy1 statement $ semi tokenParser)
    return $ if length list == 1 then head list else Seq list

statement :: Parser Stmt
statement = rawStmt <|> ifStmt <|> includeStmt <|> insertStmt

genStmt a b = do
    reserved tokenParser a
    between (char '(') (char ')') $ inParens b

rawStmt :: Parser Stmt
rawStmt = genStmt "raw" Raw

includeStmt :: Parser Stmt
includeStmt = genStmt "include" Include

insertStmt :: Parser Stmt
insertStmt = do
    reserved tokenParser "insert"
    spaces
    char '('
    x <- pwordStmt
    char ')'
    return $ Insert x

ifStmt :: Parser Stmt
ifStmt = do
    reserved tokenParser "if"
    cond <- bExpr
    stmt1 <- statement
    spaces
    reserved tokenParser "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2

inParens :: (String -> Stmt) -> Parser Stmt
inParens f = do
    x <- manyTill anyChar (lookAhead $ string ")")
    return $ f x

x <-| "repo"    = repo $ tConfig x
x <-| "parser"  = parser $ tConfig x
x <-| "name"    = name $ tConfig x
x <-| "target"  = target x
x <-| "yaml"    = yaml $ tConfig x
x <-| s         = s
