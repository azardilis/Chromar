module Chromar.MRuleParser where

import Text.Parsec
import Data.List
import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Syntax
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (makeTokenParser)

import qualified Text.Parsec.Token as Tok

data SRule = SRule
    { lexps :: [Exp]
    , rexps :: [Exp]
    , mults :: [Exp]
    , srate :: Exp
    , cond :: Exp
    } deriving (Show)
    
langDef =
    emptyDef
    { Tok.reservedOpNames = ["-->", "@", "=", "*"]
    , Tok.reservedNames = ["where"]
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

op = Tok.reservedOp lexer

name = Tok.identifier lexer

commaSep = Tok.commaSep lexer

braces = Tok.braces lexer

squares = Tok.squares lexer

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
  return (agentNm ++ "{" ++ (intercalate "," attrs) ++ "}")

ragent :: Parser (String, String)
ragent = do
  mult <- option "1" (many1 (noneOf ['*']))
  op "*"
  agentNm <- name
  attrs <- braces (commaSep attr)
  return (mult, agentNm ++ "{" ++ (intercalate "," attrs) ++ "}")

lhsParser :: Parser [String]
lhsParser = commaSep lagent

rhsParser :: Parser [(String, String)]
rhsParser = commaSep ragent

createExp :: String -> Exp
createExp exp =
    case parseExp exp of
        Left s -> error s
        Right exp -> exp

createExps :: [String] -> [Exp]
createExps exps =
    case mapM parseExp exps of
        Left s -> error s
        Right pexps -> pexps

parseRule :: Parser SRule
parseRule = do
    whiteSpace
    lhs <- lhsParser
    op "-->"
    rhs <- rhsParser
    let (multExps, ragentExps) = unzip rhs
    op "@"
    rexpr <- many1 (noneOf ['['])
    cexpr <- option "True" (squares (many1 (noneOf [']'])))
    return
        SRule
        { lexps = createExps lhs
        , rexps = createExps ragentExps
        , mults = createExps multExps
        , srate = createExp rexpr
        , cond = createExp cexpr
        }

--- for testing
contents = "A{x=x', y=ygh}, A{x=a, y=m1} --> m1 * A{x=f x} @1.0 [x + 1 + 5   ] "
  
