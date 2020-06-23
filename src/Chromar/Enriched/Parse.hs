{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Chromar.Enriched.Parse
    ( -- * Parsing
      parseErString, parseEr, parseExp, parseEither
    ) where

import Data.List (intercalate)
import Data.Set (Set)
import Data.Functor.Identity (Identity)
import qualified Data.Set as Set (empty, union, fromList)
import qualified Language.Haskell.Meta.Parse as Meta (parseExp, parsePat)
import "template-haskell" Language.Haskell.TH (Name, Exp, Pat, mkName)
import Text.Parsec ((<|>), ParsecT, ParseError, many1, noneOf, between, parse)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (Parser)
import qualified Text.Parsec.Token as Tok
    ( GenLanguageDef, TokenParser, GenTokenParser(..)
    , reservedNames, reservedOpNames, makeTokenParser
    )

import Chromar.Enriched.Syntax (SEr(..), Nm)

langDef :: Tok.GenLanguageDef String u Identity
langDef =
    emptyDef
        { Tok.reservedOpNames =
            ["$", "repeatEvery", "when", "else", "select", "aggregate"]
        , Tok.reservedNames =
            ["repeatEvery", "when", "else", "select", "aggregate"]
        }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

op :: String -> ParsecT String () Identity ()
op = Tok.reservedOp lexer

name :: ParsecT String () Identity String
name = Tok.identifier lexer

commaSep :: ParsecT String () Identity a -> ParsecT String () Identity [a]
commaSep = Tok.commaSep lexer

braces, squares :: ParsecT String () Identity a -> ParsecT String () Identity a
braces = Tok.braces lexer
squares = Tok.squares lexer

whiteSpace :: ParsecT String () Identity ()
whiteSpace = Tok.whiteSpace lexer

attr :: Parser String
attr = do
    attrNm <- name
    op "="
    expr <- many1 (noneOf ['}', ','])
    return (attrNm ++ "=" ++ expr)

lagent :: Parser String
lagent = do
    agentNm <- name
    attrs <- braces (commaSep attr)
    return (agentNm ++ "{" ++ intercalate "," attrs ++ "}")

getEsc :: String -> Set Name
getEsc "" = Set.empty
getEsc (c : cs)
    | c == '$' = Set.union (Set.fromList [mkName ident]) (getEsc rest)
    | otherwise = getEsc cs
    where
        (ident, rest) = getIdent cs ""

        getIdent "" acc = (acc, "")
        getIdent (c' : cs') acc =
            if c == '$' then (acc, cs') else getIdent cs' (acc ++ [c'])

rmEscChar :: String -> String
rmEscChar cs = [c | c <- cs, c /= '$']

hExpr :: (String -> e) -> Parser (SEr e)
hExpr f = do
    s <- Text.Parsec.between
            (Tok.symbol lexer "\'")
            (Tok.symbol lexer "\'")
            (many1 (noneOf ['\'']))
    let nms = getEsc s
    return $ XExpr nms (f $ rmEscChar s)

parseEr :: (String -> e) -> Parser (SEr e)
parseEr f = whiteSpace >>
    ( whenExpr f
    <|> repeatExpr f
    <|> obsExpr f
    <|> timeExpr
    <|> parensExpr f
    <|> hExpr f
    )

whenExpr :: (String -> e) -> Parser (SEr e)
whenExpr f = do
    op "when"
    er1 <- parseEr f
    er2 <- parseEr f
    op "else"
    er3 <- parseEr f
    return $ When er1 er2 er3

repeatExpr :: (String -> e) -> Parser (SEr e)
repeatExpr f = do
    op "repeatEvery"
    er1 <- parseEr f
    er2 <- parseEr f
    return $ Repeat er1 er2

foldExpr :: (String -> e) -> Parser (Nm, SEr e)
foldExpr f = do
    nm <- Tok.identifier lexer
    _ <- Tok.symbol lexer "."
    er' <- parseEr f
    return (nm, er')

obsExpr :: (String -> e) -> Parser (SEr e)
obsExpr f = do
    op "select"
    lat <- lagent
    op ";"
    op "aggregate"
    (nm, er1) <-
        Text.Parsec.between
            (Tok.symbol lexer "(")
            (Tok.symbol lexer ")")
            (foldExpr f)
    er2 <- parseEr f
    return $ Obs (parsePat lat) nm er1 er2

timeExpr :: Parser (SEr e)
timeExpr = do
    op "time"
    return Time

parensExpr :: (String -> e) -> Parser (SEr e)
parensExpr f = do
    Text.Parsec.between
        (Tok.symbol lexer "(")
        (Tok.symbol lexer ")")
        (parseEr f)

parsePat :: String -> Pat
parsePat s = case Meta.parsePat s of
    Left err -> error err
    Right p -> p

parseExp :: String -> Exp
parseExp s = case Meta.parseExp s of
    (Left err) -> error err
    (Right e) -> e

-- |
-- >>> parseErString "repeatEvery '5' (when '$light$ + 1' '5' else '1')"
-- Repeat (XExpr (fromList []) (LitE (IntegerL 5))) (When (XExpr (fromList []) (UInfixE (VarE light) (VarE +) (LitE (IntegerL 1)))) (XExpr (fromList []) (LitE (IntegerL 5))) (XExpr (fromList []) (LitE (IntegerL 1))))
--
-- >>> parseErString "select Leaf{m=m}; aggregate (count.'count + m') '0'"
-- Obs (RecP Leaf [(m,VarP m)]) "count" (XExpr (fromList []) (UInfixE (VarE count) (VarE +) (VarE m))) (XExpr (fromList []) (LitE (IntegerL 0)))
parseErString :: String -> SEr Exp
parseErString s = case parse (parseEr parseExp) "er" s of
    (Left err) -> error (show err)
    (Right val) -> val

-- |
-- >>> parseEither "repeatEvery '5' (when '$light$ + 1' '5' else '1')"
-- Right (Repeat (XExpr (fromList []) (LitE (IntegerL 5))) (When (XExpr (fromList []) (UInfixE (VarE light) (VarE +) (LitE (IntegerL 1)))) (XExpr (fromList []) (LitE (IntegerL 5))) (XExpr (fromList []) (LitE (IntegerL 1)))))
--
-- >>> parseEither "select Leaf{m=m}; aggregate (count.'count + m') '0'"
-- Right (Obs (RecP Leaf [(m,VarP m)]) "count" (XExpr (fromList []) (UInfixE (VarE count) (VarE +) (VarE m))) (XExpr (fromList []) (LitE (IntegerL 0))))
parseEither :: String -> Either ParseError (SEr Exp)
parseEither = parse (parseEr parseExp) "er"
