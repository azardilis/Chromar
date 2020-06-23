module Chromar.MRuleParser where

import Prelude hiding (exp)
import Text.Parsec
import Data.Functor.Identity
import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Syntax
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

import qualified Chromar.RExprs as RE

type Var = String
type AttrName = String
type Nm = String

data RAgent e = RAgent Nm [(AttrName, RE.SEr e)] deriving (Show)
data LAgent = LAgent RE.Nm [(AttrName, Var)] deriving (Show)

data ARule e =
    Rule
        { rlhs :: [LAgent]
        , rrhs :: [RAgent e]
        , mults :: [e]
        , rexpr :: RE.SEr e
        , cexpr :: RE.SEr e
        }
    deriving (Show)

data SRule =
    SRule
        { lexps :: [Exp]
        , rexps :: [Exp]
        , multExps :: [Exp]
        , srate :: Exp
        , cond :: Exp
        }
    deriving (Show)

langDef :: Tok.GenLanguageDef String u Identity
langDef =
    emptyDef
        { Tok.reservedOpNames = ["-->", "@", "=", "where"]
        , Tok.reservedNames = []
        }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

op :: String -> ParsecT String () Identity ()
op = Tok.reservedOp lexer

name :: ParsecT String () Identity String
name = Tok.identifier lexer

commaSep
    :: ParsecT String () Identity a
    -> ParsecT String () Identity [a]
commaSep = Tok.commaSep lexer

braces
    :: ParsecT String () Identity a
    -> ParsecT String () Identity a
braces = Tok.braces lexer

squares
    :: ParsecT String () Identity a
    -> ParsecT String () Identity a
squares = Tok.squares lexer

whiteSpace :: ParsecT String () Identity ()
whiteSpace = Tok.whiteSpace lexer

lattr :: Parser (AttrName, Var)
lattr = do
    attrNm <- name
    op "="
    v <- many1 (noneOf ['}', ','])
    return (attrNm, v)

rattr :: Parser (AttrName, RE.SEr Exp)
rattr = do
    attrNm <- name
    op "="
    es <- many1 (noneOf ['}', ','])
    return (attrNm, RE.parseErString es)

lagent :: Parser LAgent
lagent = do
    agentNm <- name
    attrs <- braces (commaSep lattr)
    return $ LAgent agentNm attrs

mult :: Parser String
mult = braces (many1 (noneOf ['}']))

ragent :: Parser (Exp, RAgent Exp)
ragent = do
    m <- option "1" mult
    agentNm <- name
    attrs <- braces (commaSep rattr)
    return (RE.mkExp m, RAgent agentNm attrs)

lhsParser :: Parser [LAgent]
lhsParser = commaSep lagent

rhsParser :: Parser [(Exp, RAgent Exp)]
rhsParser = commaSep ragent

dec :: Parser (String, String)
dec = do
    vName <- name
    op "="
    expr <- many1 (noneOf ",")
    return (vName, expr)

valDec :: (String, String) -> Dec
valDec (nm, sexpr) = ValD (VarP $ mkName nm) (NormalB expr) [] where
    expr = createExp sexpr

whereParser :: Parser [Dec]
whereParser = do
    op "where"
    decs <- commaSep dec
    return (map valDec decs)

createExp :: String -> Exp
createExp exp = case parseExp exp of
    Left s -> error s
    Right exp' -> exp'

createExps :: [String] -> [Exp]
createExps exps = case mapM parseExp exps of
    Left s -> error s
    Right pexps -> pexps

parseRule :: Parser (ARule Exp)
parseRule = do
    whiteSpace
    lhs <- lhsParser
    op "-->"
    rhs <- rhsParser
    let (mults', ragents) = unzip rhs
    op "@"
    rexpr' <- many1 (noneOf ['['])
    cexpr' <- option "'True'" (squares (many1 (noneOf [']'])))
    return
        Rule
            { rlhs = lhs
            , rrhs = ragents
            , mults = mults'
            , rexpr = RE.parseErString rexpr'
            , cexpr = RE.parseErString cexpr'
            }

--- for testing
contents :: String
contents = "A{x=x'}--> A{x='x+1'} @'$na$'"

go :: ARule Exp
go = case parse parseRule "rule" contents of
    (Left err) -> error (show err)
    (Right val) -> val
