{-# LANGUAGE PackageImports #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | Quasiquoting enriched expressions.
module Chromar.Enriched.TH
    ( -- * Quoting
      er, eval
    , mkErApp'
    ) where

import Data.Set (Set, member)
import "template-haskell" Language.Haskell.TH
    (Q, Name, Dec(..), Exp(..), Pat(..), Body(..), Clause(..), Stmt(..), mkName)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.ParserCombinators.Parsec (parse)

import Internal.RuleQuotes (lVarExp)
import Chromar.Enriched.Syntax (SEr(..), Nm)
import Chromar.Enriched.Parse (parseExp, parseEr)

lExp :: Set Name -> Exp -> Exp
lExp = lVarExp (\nm nms -> if nm `member` nms then mkErApp nm else VarE nm)

-- | Evaluate the syntax of enriched expressions as expressions in the host
-- language.
--
-- >>> ppr $ eval Time
-- time
eval :: SEr Exp -> Exp
eval Time = VarE $ mkName "time"
eval (XExpr nms e) = AppE (VarE $ mkName "mkEr") (mkLiftExp nms e)
eval (When er1 er2 er3) = mkWhenExp (eval er1) (eval er2) (eval er3)
eval (Repeat er1 er2) = mkRepeatExp (eval er1) (eval er2)
eval (Obs lat nm er1 er2) = mkObsExp' lat nm (eval er1) (eval er2)

-- | Parse an er, an enriched expression.
--
-- >>> render $ erExp "repeatEvery '5' (when '$light$ + 1' '5' else '1')"
-- repeatEvery (mkEr (\_ _ -> 5)) (orElse (when (mkEr (\_ _ -> light + 1)) (mkEr (\_ _ -> 5))) (mkEr (\_ _ -> 1)))
--
-- >>> render $ erExp "select Leaf{m=m}; aggregate (count.'count + m') '0'"
-- mkEr (let go [] count s t = count
--           go ((Leaf {m = m}) : as) count s t = go as (at (mkEr (\_ _ -> count + m)) s t) s t
--        in \s t -> go [el | el@(Leaf {m = m}) <- toList s] (at (mkEr (\_ _ -> 0)) s t) s t)
erExp :: String -> Q Exp
erExp s = case parse (parseEr parseExp) "er" s of
    Left err -> error (show err)
    Right e -> return $ eval e

-- | The er quasiquoter may be used produce enriched expressions.
er :: QuasiQuoter
er =
    QuasiQuoter
        { quoteExp = erExp
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
mkSelect pat = CompE [bindStmt, retStmt] where
    bindStmt =
        BindS
            (AsP (mkName "el") pat)
            (AppE (VarE $ mkName "toList") (VarE $ mkName "s"))
    retStmt = NoBindS (VarE $ mkName "el")

-- |
-- >>> ppr stPat
-- s
stPat :: Pat
stPat = VarP $ mkName "s"

-- |
-- >>> ppr timePat
-- t
timePat :: Pat
timePat = VarP $ mkName "t"

-- |
-- >>> ppr stExp
-- s
stExp :: Exp
stExp = VarE $ mkName "s"

-- |
-- >>> ppr timeExp
-- t
timeExp :: Exp
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

-- |
-- >>> ppr . mkErApp $ mkName "x"
-- (at x s t)
mkErApp :: Name -> Exp
mkErApp nm = mkErApp' (VarE nm)

-- |
-- >>> ppr $ mkErApp' (VarE $ mkName "x")
-- (at x s t)
-- >>> ppr $ mkErApp' (mkErApp $ mkName "x")
-- (at (at x s t) s t)
mkErApp' :: Exp -> Exp
mkErApp' e =
    ParensE
        (AppE
             (AppE (AppE (VarE $ mkName "at") e) (VarE $ mkName "s"))
             (VarE $ mkName "t"))

-- $setup
-- >>> import "template-haskell" Language.Haskell.TH (ppr, runQ)
-- >>> render = fmap ppr . runQ
