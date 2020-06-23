{-# LANGUAGE PackageImports #-}

module Chromar.Enriched.TH
    ( -- * Quoting
      er, quoteEr
    ) where

import Data.Set (Set, member)
import "template-haskell" Language.Haskell.TH
    (Q, Name, Dec(..), Exp(..), Pat(..), Body(..), Clause(..), Stmt(..), mkName)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.ParserCombinators.Parsec (parse)

import Internal.RuleQuotes (lExp, mkErApp')
import Chromar.Enriched.Syntax (SEr(..), Nm)
import Chromar.Enriched.Parse (parseExp, parseEr)

quoteEr :: SEr Exp -> Exp
quoteEr Time = VarE $ mkName "time"
quoteEr (XExpr nms e) = AppE (VarE $ mkName "mkEr") (mkLiftExp nms e)
quoteEr (When er1 er2 er3) = mkWhenExp (quoteEr er1) (quoteEr er2) (quoteEr er3)
quoteEr (Repeat er1 er2) = mkRepeatExp (quoteEr er1) (quoteEr er2)
quoteEr (Obs lat nm er1 er2) = mkObsExp' lat nm (quoteEr er1) (quoteEr er2)

-- |
-- >>> runQ $ erQuoter "repeatEvery '5' (when '$light$ + 1' '5' else '1')"
-- AppE (AppE (VarE repeatEvery) (AppE (VarE mkEr) (LamE [WildP,WildP] (LitE (IntegerL 5))))) (AppE (AppE (VarE orElse) (AppE (AppE (VarE when) (AppE (VarE mkEr) (LamE [WildP,WildP] (UInfixE (VarE light) (VarE +) (LitE (IntegerL 1)))))) (AppE (VarE mkEr) (LamE [WildP,WildP] (LitE (IntegerL 5)))))) (AppE (VarE mkEr) (LamE [WildP,WildP] (LitE (IntegerL 1)))))
--
-- >>> runQ $ erQuoter "select Leaf{m=m}; aggregate (count.'count + m') '0'"
-- AppE (VarE mkEr) (LetE [FunD go [Clause [ListP [],VarP count,VarP s,VarP t] (NormalB (VarE count)) [],Clause [ParensP (UInfixP (RecP Leaf [(m,VarP m)]) : (VarP as)),VarP count,VarP s,VarP t] (NormalB (AppE (AppE (AppE (AppE (VarE go) (VarE as)) (ParensE (AppE (AppE (AppE (VarE at) (AppE (VarE mkEr) (LamE [WildP,WildP] (UInfixE (VarE count) (VarE +) (VarE m))))) (VarE s)) (VarE t)))) (VarE s)) (VarE t))) []]] (LamE [VarP s,VarP t] (AppE (AppE (AppE (AppE (VarE go) (CompE [BindS (AsP el (RecP Leaf [(m,VarP m)])) (AppE (VarE toList) (VarE s)),NoBindS (VarE el)])) (ParensE (AppE (AppE (AppE (VarE at) (AppE (VarE mkEr) (LamE [WildP,WildP] (LitE (IntegerL 0))))) (VarE s)) (VarE t)))) (VarE s)) (VarE t))))
--
erQuoter :: String -> Q Exp
erQuoter s = case parse (parseEr parseExp) "er" s of
    Left err -> error (show err)
    Right e -> return $ quoteEr e

-- | Parse the quote into Er then create the functions per the semantics.
er :: QuasiQuoter
er =
    QuasiQuoter
        { quoteExp = erQuoter
        , quotePat = undefined
        , quoteDec = undefined
        , quoteType = undefined
        }

-- | Takes function and list of args and returns the expression for function
-- application to the args.
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

-- $setup
-- >>> import "template-haskell" Language.Haskell.TH (runQ)
