module Chromar.MRuleParser where

import Text.Parsec
import Data.List
import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Syntax
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (makeTokenParser)

import qualified Text.Parsec.Token as Tok

import qualified Chromar.RExprs as RE


data RAgent e =
    RAgent Nm
           [(AttrName, e)]
    deriving (Show)

data LAgent =
    LAgent Nm
           [(AttrName, Var)] deriving (Show)

data ARule e = Rule
    { lhs :: [LAgent]
    , rhs :: [RAgent e]
    , rexpr :: e
    , cexpr :: e
    } deriving (Show)


langDef =
    emptyDef
    { Tok.reservedOpNames = ["-->", "@", "=", "where"]
    , Tok.reservedNames = []
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
  return (agentNm ++ "{" ++ intercalate "," attrs ++ "}")

mult :: Parser String
mult = braces (many1 (noneOf ['}']))

ragent :: Parser (String, String)
ragent = do
  m <- option "1" mult
  agentNm <- name
  attrs <- braces (commaSep attr)
  return (m, agentNm ++ "{" ++ intercalate "," attrs ++ "}")

lhsParser :: Parser [String]
lhsParser = commaSep lagent

rhsParser :: Parser [(String, String)]
rhsParser = commaSep ragent

dec :: Parser (String, String)
dec = do
  vName <- name
  op "="
  expr <- many1 (noneOf ",")
  return (vName, expr)

valDec :: (String, String) -> Dec
valDec (nm, sexpr) = ValD (VarP $ mkName nm) (NormalB expr) []
  where
    expr = createExp sexpr
    
whereParser :: Parser [Dec]
whereParser = do
  op "where"
  decs <- commaSep dec
  return (map valDec decs)

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

parseRule :: Parser (ARule Exp)
parseRule = do
    whiteSpace
    lhs <- lhsParser
    op "-->"
    rhs <- rhsParser
    let (multExps, ragentExps) = unzip rhs
    op "@"
    rexpr <- many1 (noneOf ['['])
    cexpr <- option "{True}" (squares (many1 (noneOf [']'])))
    wdecs <- option [] whereParser
    return 
        SRule
        { lexps = createExps lhs
        , rexps = createExps ragentExps
        , mults = createExps multExps
        , srate = RE.mkErApp' (RE.quoteEr $ RE.parseErString rexpr)
        , cond = RE.mkErApp' (RE.quoteEr $ RE.parseErString cexpr)
        , decs = wdecs         
        }

--- for testing
contents = "A{x=x', y=ygh}, A{x=a, y=m1} --> A{x=f x} @{1 + 2} [{True}]"

go = case parse parseRule "rule" contents of
  (Left err) -> error (show err)
  (Right val) -> val
