{-# LANGUAGE TemplateHaskell #-}

module RExprs where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec
import Data.List
import Language.Haskell.Meta.Parse
import Text.Parsec
import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Syntax
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (makeTokenParser)
import Data.Fixed

import qualified Text.Parsec.Token as Tok

import Chromar.Multiset
import Chromar.Core

data ErF a b = ErF { at :: Multiset a -> Time -> b }

repeatEvery :: ErF a Time -> ErF a b -> ErF a b
repeatEvery et er =
    ErF
    { at = \s t -> at er s (mod' t (at et s t))
    }

when :: ErF a Bool -> ErF a b -> ErF a (Maybe b)
when eb er =
    ErF
    { at =
        \s t ->
             if (at eb s t)
                 then Just (at er s t)d
                 else Nothing
    }

orElse :: ErF a (Maybe b) -> ErF a b -> ErF a b
e1 `orElse` e2 =
    ErF
    { at =
        \s t ->
             case (at e1 s t) of
                 Just x -> x
                 Nothing -> at e2 s t
    }

time =
    ErF
    { at = \s t -> t
    }

obs :: (a -> Bool) -> (a -> b -> b) -> b -> ErF a b
obs f comb i =
    ErF
    { at = \s t -> aggregate comb i . select f $ s
    }


select :: (a -> Bool) -> Multiset a -> Multiset a
select f = filter (\(el, _) -> f el)

aggregate :: (a -> b -> b) -> b -> Multiset a -> b
aggregate f i s = foldr f i (toList s)

mkEr :: (Multiset a -> Time -> b) -> ErF a b
mkEr f = ErF { at = f }


type Nm = String

data Er
    = HExpr (Set Name)
            Exp
    | Time
    | When Er
           Er
           Er
    | Repeat Er
             Er
    | Obs Nm
          Er
          Er
    deriving (Show)

langDef =
    emptyDef
    { Tok.reservedOpNames = ["$"]
    , Tok.reservedNames = ["repeatEvery", "when", "else", "select", "aggregate", "time"]
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

op = Tok.reservedOp lexer

name = Tok.identifier lexer

commaSep = Tok.commaSep lexer

braces = Tok.braces lexer

squares = Tok.squares lexer

whiteSpace = Tok.whiteSpace lexer


getEsc :: String -> Set Name
getEsc "" = Set.empty
getEsc (c:cs)
    | c == '$' = Set.union (Set.fromList [mkName ident]) (getEsc rest)
    | otherwise = getEsc cs
  where
    (ident, rest) = getIdent cs ""
    getIdent "" acc = (acc, "")
    getIdent (c:cs) acc =
        if c == '$'
            then (acc, cs)
            else getIdent cs (acc ++ [c])

rmEscChar :: String -> String
rmEscChar cs = [c | c <- cs, not (c == '$')]

mkExp :: String -> Exp
mkExp s = case parseExp s of
  (Left err) -> error err
  (Right e) -> e

hExpr :: Parser Er
hExpr = do
  Tok.symbol lexer "{"
  s <- many1 (noneOf ['}'])
  Tok.symbol lexer "}"
  let nms = getEsc s
  case parseExp (rmEscChar s) of
    Left err -> error (err)
    Right e -> return (HExpr nms e)

parseEr :: Parser Er
parseEr =
    Tok.parens lexer whenExpr <|> repeatExpr <|> obsExpr <|> timeExpr <|> parensExpr <|>
    hExpr
    
whenExpr :: Parser Er
whenExpr = do
  op "when"
  er1 <- parseEr
  er2 <- parseEr
  op "else"
  er3 <- parseEr
  return $ When er1 er2 er3

repeatExpr :: Parser Er
repeatExpr = do
  op "repeatEvery"
  er1 <- parseEr
  er2 <- parseEr
  return $ Repeat er1 er2

obsExpr :: Parser Er
obsExpr = do
  op "select"
  nm <- Tok.identifier lexer
  op ";"
  op "aggregate"
  er1 <- parseEr
  er2 <- parseEr
  return $ Obs nm er1 er2

timeExpr :: Parser Er
timeExpr = do
  op "time"
  return $ Time

parensExpr :: Parser Er
parensExpr = do
  er <- Text.Parsec.between (Tok.symbol lexer "(") (Tok.symbol lexer ")") parseEr
  return er

mkFApp = undefined

tStmt = undefined

tMExp = undefined

tName = undefined

tExp :: Set Name -> Exp -> Exp
tExp nms var@(VarE nm) =
    if Set.member nm nms
        then mkFApp nm
        else var             
tExp nms (AppE e1 e2) = AppE (tExp nms e1) (tExp nms e2)
tExp nms (TupE exps) = TupE (map (tExp nms) exps)
tExp nms (ListE exps) = ListE (map (tExp nms) exps)
tExp nms (UInfixE e1 e2 e3) = UInfixE (tExp nms e1) (tExp nms e2) (tExp nms e3)
tExp nms (ParensE e) = ParensE (tExp nms e)
tExp nms (LamE pats e) = LamE pats (tExp nms e)
tExp nms (CompE stmts) = CompE (map (tStmt nms) stmts)
tExp nms (InfixE me1 e me2) =
    InfixE (tMExp nms me1) (tExp nms e) (tMExp nms me2)
tExp nms (LitE lit) = LitE lit
tExp nms (ConE nm) = ConE nm
tExp nms (RecConE nm fexps) = RecConE nm (tFExp nms fexps)
tExp nms _ = undefined

quoteEr :: Er -> Q Exp
quoteEr Time = [| time |]
quoteEr (HExpr nms e) = undefined
quoteEr (When er1 er2 er3) = undefined
quoteEr (Repeat er1 er2) = undefined
quoteEr (Obs nm er1 er2) = undefined

erQuoter :: String -> Q Exp
erQuoter = undefined
---- parse the quote into Er then create the functions per the semantics


------------- testing
contents = "repeatEvery {5} (when {$light + 1} {5} else {1})"

go = case parse parseEr "er" contents of
  (Left err) -> error (show err)
  (Right val) -> val
  
  
  


