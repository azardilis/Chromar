{-# LANGUAGE PackageImports #-}

module Chromar.RExprs where

import Prelude hiding (exp)
import Data.Fixed (mod')
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Set (Set, member)
import Data.Functor.Identity
import qualified Data.Set as Set
import Language.Haskell.Meta.Parse
import "template-haskell" Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Tok

import Chromar.Core
import Chromar.Multiset
import Internal.RuleQuotes (lExp, mkErApp')

type Nm = String

{- syntactic Er's -}
data SEr e
    = XExpr (Set Name) e
    | Time
    | When (SEr e) (SEr e) (SEr e)
    | Repeat (SEr e) (SEr e)
    | Obs Pat Nm (SEr e) (SEr e)
    deriving (Show)

{- semantic Er's -}
newtype Er a b = Er { at :: Multiset a -> Time -> b }

mmod :: Real a => a -> a -> a
mmod n m
    | m <=0 = 0
    | otherwise = mod' n m

zipEr2 :: Er a b -> Er a c -> Er a (b, c)
zipEr2 e1 e2 =
    Er
    { at = \s t -> (at e1 s t, at e2 s t)
    }

zipEr3 :: Er a b -> Er a c -> Er a d -> Er a (b, c, d)
zipEr3 e1 e2 e3 =
    Er
    { at = \s t -> (at e1 s t, at e2 s t, at e3 s t)
    }

zipEr4 :: Er a b -> Er a b1 -> Er a b2 -> Er a b3 -> Er a (b, b1, b2, b3)
zipEr4 e1 e2 e3 e4 =
    Er
    { at = \s t -> (at e1 s t, at e2 s t, at e3 s t, at e4 s t)
    }

zipEr5
    :: Er a b
    -> Er a b1
    -> Er a b2
    -> Er a b3
    -> Er a b4
    -> Er a (b, b1, b2, b3, b4)
zipEr5 e1 e2 e3 e4 e5 =
    Er
    { at = \s t -> (at e1 s t, at e2 s t, at e3 s t, at e4 s t, at e5 s t)
    }

repeatEvery :: Er a Time -> Er a b -> Er a b
repeatEvery et er' =
    Er
    { at = \s t -> at er' s (mmod t (at et s t))
    }

when :: Er a Bool -> Er a b -> Er a (Maybe b)
when eb er' =
    Er
    { at =
        \s t ->
             if at eb s t
                 then Just (at er' s t)
                 else Nothing
    }

orElse :: Er a (Maybe b) -> Er a b -> Er a b
e1 `orElse` e2 =
    Er
    { at = \s t -> fromMaybe (at e2 s t) (at e1 s t)
    }

time :: Er a Time
time =
    Er
    { at = \_ t -> t
    }

obs :: (a -> Bool) -> Er a (a -> b -> b) -> Er a b -> Er a b
obs f comb i =
    Er
    { at = \s t -> aggregate (at comb s t) (at i s t) . select f $ s
    }

select :: (a -> Bool) -> Multiset a -> Multiset a
select f = filter (\(el, _) -> f el)

aggregate :: (a -> b -> b) -> b -> Multiset a -> b
aggregate f i s = foldr f i (toList s)

mkEr :: (Multiset a -> Time -> b) -> Er a b
mkEr f = Er { at = f }

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

parseP :: String -> Pat
parseP s = case parsePat s of
    Left err -> error err
    Right p -> p

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

mkExp :: String -> Exp
mkExp s = case parseExp s of
    (Left err) -> error err
    (Right e) -> e

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
    return $ Obs (parseP lat) nm er1 er2

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

{-
   takes function and list of args and returns the expression for
   function application to the args
-}
mkFApp :: Exp -> [Exp] -> Exp
mkFApp _ [] = undefined
mkFApp f (e:exps) = foldr (flip AppE) (AppE f e) (reverse exps)

mkSelect :: Pat -> Exp
mkSelect pat = CompE [bindStmt, retStmt]
    where
        bindStmt =
            BindS
                (AsP (mkName "el") pat)
                (AppE (VarE $ mkName "toList") (VarE $ mkName "s"))
        retStmt = NoBindS (VarE $ mkName "el")

stPat, timePat :: Pat
stPat = VarP $ mkName "s"
timePat = VarP $ mkName "t"

stExp, timeExp :: Exp
stExp = VarE $ mkName "s"
timeExp = VarE $ mkName "t"

mkLiftExp :: Set Name -> Exp -> Exp
mkLiftExp nms body = LamE args (lExp nms body) where
    args =
        [ let s = mkName "s" in if s `member` nms then VarP s else WildP
        , let t = mkName "t" in if t `member` nms then VarP t else WildP
        ]

mkWhenExp :: Exp -> Exp -> Exp -> Exp
mkWhenExp eb e1 = AppE (AppE (VarE $ mkName "orElse") whenE) where
    whenE = AppE (AppE (VarE $ mkName "when") eb) e1

mkRepeatExp :: Exp -> Exp -> Exp
mkRepeatExp et = AppE (AppE (VarE $ mkName "repeatEvery") et)

mkFoldF :: Pat -> Nm -> Exp -> Dec
mkFoldF pat nm combE = FunD (mkName "go") [emptyClause, nonemptyClause] where
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
mkObsExp' pat nm combE initE = AppE (VarE $ mkName "mkEr") (LetE decs e) where
    decs = [mkFoldF pat nm combE]
    e =
        LamE
            [stPat, timePat]
            (mkFApp
                 (VarE $ mkName "go")
                 [mkSelect pat, mkErApp' initE, stExp, timeExp])

quoteEr :: SEr Exp -> Exp
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
contents :: String
contents = "repeatEvery '5' (when '$light$ + 1' '5' else '1')"

contents' :: String
contents' = "select Leaf{m=m}; aggregate (count.'count + m') '0'"

go :: SEr Exp
go = case parse (parseEr mkExp) "er" contents' of
    (Left err) -> error (show err)
    (Right val) -> val

parseErString :: String -> SEr Exp
parseErString s = case parse (parseEr mkExp) "er" s of
    (Left err) -> error (show err)
    (Right val) -> val
