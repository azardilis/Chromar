{-# LANGUAGE PackageImports #-}

module Internal.RuleQuotes (tuplify, tuplify2, lExp, mkErApp, mkErApp') where

import Prelude hiding (exp)
import Data.Set (Set)
import qualified Data.Set as Set
import "template-haskell" Language.Haskell.TH (Name, Stmt(..), Exp(..), mkName)

tuplify :: Name -> Exp -> Exp -> Exp
tuplify s lhs r = TupE [lhs, VarE s, r]

tuplify2 :: Exp -> Exp -> Exp
tuplify2 m ar = TupE [m, ar]

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
lExp nms (CompE stmts) = CompE (map (tStmt nms) stmts) where
    tStmt nms' (BindS p e) = BindS p (lExp nms' e)
    tStmt nms' (NoBindS e) = NoBindS (lExp nms' e)
    tStmt _ LetS{} = error "Unexpected LetS"
    tStmt _ ParS{} = error "Unexpected ParS"
lExp nms (InfixE me1 e me2) =
    InfixE (fmap (lExp nms) me1) (lExp nms e) (fmap (lExp nms) me2)
lExp _ (LitE lit) = LitE lit
lExp _ (ConE nm) = ConE nm
lExp nms (RecConE nm fexps) = RecConE nm (map (tFExp nms) fexps) where
    tFExp nms' (nm', exp) = (nm', lExp nms' exp)
lExp nms (CondE e1 e2 e3) = CondE (lExp nms e1) (lExp nms e2) (lExp nms e3)
lExp _nms _ = undefined

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
