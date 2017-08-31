module Chromar.RExprs where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec
import Data.List
import Text.Parsec
import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Syntax
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (makeTokenParser)
import Data.Fixed
import Data.Maybe

import qualified Text.Parsec.Token as Tok

import Chromar.Multiset
import Chromar.Core

data ErF a b = ErF { at :: Multiset a -> Time -> b }

mmod :: Real a => a -> a -> a
mmod n m
  | m <=0 = 0
  | otherwise = mod' n m          

zipEr2 :: ErF a b -> ErF a c -> ErF a (b, c)
zipEr2 e1 e2 =
    ErF
    { at = \s t -> (at e1 s t, at e2 s t)
    }

zipEr3 :: ErF a b -> ErF a c -> ErF a d -> ErF a (b, c, d)
zipEr3 e1 e2 e3 =
    ErF
    { at = \s t -> (at e1 s t, at e2 s t, at e3 s t)
    }

zipEr4 :: ErF a b -> ErF a b1 -> ErF a b2 -> ErF a b3 -> ErF a (b, b1, b2, b3)
zipEr4 e1 e2 e3 e4 =
    ErF
    { at = \s t -> (at e1 s t, at e2 s t, at e3 s t, at e4 s t)
    }

zipEr5
    :: ErF a b
    -> ErF a b1
    -> ErF a b2
    -> ErF a b3
    -> ErF a b4
    -> ErF a (b, b1, b2, b3, b4)
zipEr5 e1 e2 e3 e4 e5 =
    ErF
    { at = \s t -> (at e1 s t, at e2 s t, at e3 s t, at e4 s t, at e5 s t)
    }

repeatEvery :: ErF a Time -> ErF a b -> ErF a b
repeatEvery et er =
    ErF
    { at = \s t -> at er s (mmod t (at et s t))
    }

when :: ErF a Bool -> ErF a b -> ErF a (Maybe b)
when eb er =
    ErF
    { at =
        \s t ->
             if at eb s t
                 then Just (at er s t)
                 else Nothing
    }

orElse :: ErF a (Maybe b) -> ErF a b -> ErF a b
e1 `orElse` e2 =
    ErF
    { at = \s t -> fromMaybe (at e2 s t) (at e1 s t)
    }

time =
    ErF
    { at = \s t -> t
    }

obs :: (a -> Bool) -> ErF a (a -> b -> b) -> ErF a b -> ErF a b
obs f comb i =
    ErF
    { at = \s t -> aggregate (at comb s t) (at i s t) . select f $ s
    }

select :: (a -> Bool) -> Multiset a -> Multiset a
select f = filter (\(el, _) -> f el)

aggregate :: (a -> b -> b) -> b -> Multiset a -> b
aggregate f i s = foldr f i (toList s)

mkEr :: (Multiset a -> Time -> b) -> ErF a b
mkEr f = ErF { at = f }

type Nm = String

data Er e
    = XExpr (Set Name)
            e
    | Time
    | When (Er e)
           (Er e)
           (Er e)
    | Repeat (Er e)
             (Er e)
    | Obs Pat
          Nm
          (Er e)
          (Er e)
    deriving (Show)

langDef =
    emptyDef
    { Tok.reservedOpNames = ["$"]
    , Tok.reservedNames =
        ["repeatEvery", "when", "else", "select", "aggregate", "time"]
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

parseP :: String -> Pat
parseP s = case parsePat s of
  Left err -> error err
  Right p -> p

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
rmEscChar cs = [c | c <- cs, c /= '$']

mkExp :: String -> Exp
mkExp s = case parseExp s of
  (Left err) -> error err
  (Right e) -> e

hExpr :: (String -> e) -> Parser (Er e)
hExpr f = do
  Tok.symbol lexer "'"
  s <- many1 (noneOf ['\''])
  Tok.symbol lexer "\'"
  let nms = getEsc s
  return $ XExpr nms (f $ rmEscChar s)
  -- case (f $ rmEscChar s) of
  --   Left err -> error err
  --   Right e -> return (XExpr nms e)

parseEr :: (String -> e) -> Parser (Er e)
parseEr f =
    (whenExpr f) <|> (repeatExpr f) <|> (obsExpr f) <|> timeExpr <|> (parensExpr f) <|>
    (hExpr f) <|> (spaceExpr f)

whenExpr :: (String -> e) -> Parser (Er e)
whenExpr f = do
  op "when"
  er1 <- parseEr f
  er2 <- parseEr f
  op "else"
  er3 <- parseEr f
  return $ When er1 er2 er3

repeatExpr :: (String -> e) -> Parser (Er e)
repeatExpr f = do
  op "repeatEvery"
  er1 <- parseEr f
  er2 <- parseEr f
  return $ Repeat er1 er2

obsExpr :: (String -> e) -> Parser (Er e)
obsExpr f = do
    op "select"
    lat <- lagent
    op ";"
    op "aggregate"
    nm <-
        Text.Parsec.between
            (Tok.symbol lexer "(")
            (Tok.symbol lexer ")")
            (Tok.identifier lexer)
    Tok.symbol lexer "."
    er1 <- parseEr f
    Tok.symbol lexer ","
    er2 <- parseEr f
    return $ Obs (parseP lat) nm er1 er2

timeExpr :: Parser (Er e)
timeExpr = do
  op "time"
  return Time

parensExpr :: (String -> e) -> Parser (Er e)
parensExpr f = do
    er <- Text.Parsec.between (Tok.symbol lexer "(") (Tok.symbol lexer ")") (parseEr f)
    return er

spaceExpr :: (String -> e) -> Parser (Er e)
spaceExpr f = do
  whiteSpace
  er <- parseEr f
  whiteSpace
  return er

mkErApp :: Name -> Exp
mkErApp nm =
    ParensE
        (AppE
             (AppE (AppE (VarE $ mkName "at") (VarE nm)) (VarE $ mkName "s"))
             (VarE $ mkName "t"))

mkErApp' :: Exp -> Exp
mkErApp' e =
    ParensE
        (AppE
             (AppE (AppE (VarE $ mkName "at") e) (VarE $ mkName "s"))
             (VarE $ mkName "t"))

{-
    takes function and args and returns the expression for
   function application to the args
-}
mkFApp :: Exp -> [Exp] -> Exp
mkFApp f [] = undefined
mkFApp f (e:exps) = foldr (\x acc -> AppE acc x) (AppE f e) (reverse exps)

mkSelect :: Pat -> Exp
mkSelect pat = CompE [bindStmt, retStmt]
  where
    bindStmt =
        BindS
            (AsP (mkName "el") pat)
            (AppE (VarE $ mkName "toList") (VarE $ mkName "s"))
    retStmt = NoBindS (VarE $ mkName "el")

stPat = VarP $ mkName "s"

stExp = VarE $ mkName "s"

timePat = VarP $ mkName "t"

timeExp = VarE $ mkName "t"

lExp :: Set Name -> Exp -> Exp
lExp nms var@(VarE nm) =
    if Set.member nm nms
        then mkErApp nm
        else var
lExp nms (AppE e1 e2) = AppE (lExp nms e1) (lExp nms e2)
lExp nms (TupE exps) = TupE (map (lExp nms) exps)
lExp nms (ListE exps) = ListE (map (lExp nms) exps)
lExp nms (UInfixE e1 e2 e3) = UInfixE (lExp nms e1) (lExp nms e2) (lExp nms e3)
lExp nms (ParensE e) = ParensE (lExp nms e)
lExp nms (LamE pats e) = LamE pats (lExp nms e)
lExp nms (CompE stmts) = CompE (map (tStmt nms) stmts)
  where
    tStmt nms (BindS p e) = BindS p (lExp nms e)
    tStmt nms (NoBindS e) = NoBindS (lExp nms e)
lExp nms (InfixE me1 e me2) =
    InfixE (fmap (lExp nms) me1) (lExp nms e) (fmap (lExp nms) me2)
lExp nms (LitE lit) = LitE lit
lExp nms (ConE nm) = ConE nm
lExp nms (RecConE nm fexps) = RecConE nm (map (tFExp nms) fexps)
  where
    tFExp nms (nm, exp) = (nm, lExp nms exp)
lExp nms _ = undefined

mkLiftExp :: Set Name -> Exp -> Exp
mkLiftExp nms body = LamE args (lExp nms body)
  where
    args = [VarP $ mkName "s", VarP $ mkName "t"]

mkWhenExp :: Exp -> Exp -> Exp -> Exp
mkWhenExp eb e1 e2 = AppE (AppE (VarE $ mkName "orElse") whenE) e2
  where
    whenE = AppE (AppE (VarE $ mkName "when") eb) e1

mkRepeatExp :: Exp -> Exp -> Exp
mkRepeatExp et e = AppE (AppE (VarE $ mkName "repeatEvery") et) e

mkFoldF :: Pat -> Nm -> Exp -> Dec
mkFoldF pat nm combE = FunD (mkName "go") [emptyClause, nonemptyClause]
  where
    accPat = VarP $ mkName nm
    consPat = ParensP (UInfixP pat (mkName ":") (VarP $ mkName "as"))
    recGo =
        mkFApp
            (VarE $ mkName "go")
            [VarE $ mkName "as", mkErApp' combE, stExp, timeExp]
    emptyClause =
        Clause
            [ListP [], VarP $ mkName nm, VarP $ mkName "s", VarP $ mkName "t"]
            (NormalB (VarE $ mkName nm))
            []
    nonemptyClause = Clause [consPat, accPat, stPat, timePat] (NormalB recGo) []

mkObsExp' :: Pat -> Nm -> Exp -> Exp -> Exp
mkObsExp' pat nm combE initE = AppE (VarE $ mkName "mkEr") (LetE decs e)
  where
    decs = [mkFoldF pat nm combE]
    e =
        LamE
            [stPat, timePat]
            (mkFApp
                 (VarE $ mkName "go")
                 [mkSelect pat, mkErApp' initE, stExp, timeExp])

quoteEr :: Er Exp -> Exp
quoteEr Time = VarE $ mkName "time"
quoteEr (XExpr nms e) = AppE (VarE $ mkName "mkEr") (mkLiftExp nms e)
quoteEr (When er1 er2 er3) = mkWhenExp (quoteEr er1) (quoteEr er2) (quoteEr er3)
quoteEr (Repeat er1 er2) = mkRepeatExp (quoteEr er1) (quoteEr er2)
quoteEr (Obs lat nm er1 er2) = mkObsExp' lat nm (quoteEr er1) (quoteEr er2)

erQuoter :: String -> Q Exp
erQuoter s = case parse (parseEr mkExp) "er" s of
  Left err -> error (show err)
  Right e -> return $ quoteEr e
---- parse the quote into Er then create the functions per the semantics

er :: QuasiQuoter
er =
    QuasiQuoter
    { quoteExp = erQuoter
    , quotePat = undefined
    , quoteDec = undefined
    , quoteType = undefined
    }

------------- testing
contents = "repeatEvery '5' (when '$light$ + 1' '5' else '1')"

contents' = "select Leaf{m=m}; aggregate(count).'count + m', '0'"

go = case parse (parseEr mkExp) "er" contents' of
  (Left err) -> error (show err)
  (Right val) -> val

parseErString :: String -> Er Exp
parseErString s = case parse (parseEr mkExp) "er" s of
  (Left err) -> error (show err)
  (Right val) -> val
