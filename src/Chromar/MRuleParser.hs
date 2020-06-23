{-# LANGUAGE PackageImports #-}

module Chromar.MRuleParser
    ( SRule(..), ARule(..), LAgent(..), RAgent(..), Nm
    , langDef, lexer, op, name, commaSep, braces, squares, whiteSpace
    , lattr, rattr, lagent, mult, ragent
    , dec, valDec
    , lhsParser, rhsParser, whereParser, parseRule
    , createExps
    ) where

import Prelude hiding (exp)
import Text.Parsec (ParsecT, noneOf, many1, option)
import Data.Functor.Identity (Identity)
import qualified Language.Haskell.Meta.Parse as Meta (parseExp)
import "template-haskell" Language.Haskell.TH.Syntax
    (Dec(..), Exp(..), Pat(..), Body(..), mkName)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
    ( GenLanguageDef, TokenParser, GenTokenParser(..)
    , reservedNames, reservedOpNames, makeTokenParser
    )
import Chromar.Enriched.Syntax (SEr)
import Chromar.Enriched.Parse (parseErString, parseExp)

type Var = String
type AttrName = String
type Nm = String

data RAgent e = RAgent Nm [(AttrName, SEr e)] deriving (Show)
data LAgent = LAgent Nm [(AttrName, Var)] deriving (Show)

data ARule e =
    Rule
        { rlhs :: [LAgent]
        , rrhs :: [RAgent e]
        , mults :: [e]
        , rexpr :: SEr e
        , cexpr :: SEr e
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

rattr :: Parser (AttrName, SEr Exp)
rattr = do
    attrNm <- name
    op "="
    es <- many1 (noneOf ['}', ','])
    return (attrNm, parseErString es)

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
    return (parseExp m, RAgent agentNm attrs)

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
    return $ valDec <$> decs

createExp :: String -> Exp
createExp exp = case Meta.parseExp exp of
    Left s -> error s
    Right exp' -> exp'

createExps :: [String] -> [Exp]
createExps exps = case traverse Meta.parseExp exps of
    Left s -> error s
    Right pexps -> pexps

-- |
-- >>> parse parseRule "rule" "A{x=x'}--> A{x='x+1'} @'$na$'"
-- Right (Rule {rlhs = [LAgent "A" [("x","x'")]], rrhs = [RAgent "A" [("x",XExpr (fromList []) (UInfixE (VarE x) (VarE +) (LitE (IntegerL 1))))]], mults = [LitE (IntegerL 1)], rexpr = XExpr (fromList []) (VarE na), cexpr = XExpr (fromList []) (ConE True)})
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
            , rexpr = parseErString rexpr'
            , cexpr = parseErString cexpr'
            }

-- $setup
-- >>> import Text.Parsec (parse)
