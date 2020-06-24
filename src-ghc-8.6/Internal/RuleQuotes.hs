{-# LANGUAGE PackageImports #-}

module Internal.RuleQuotes (tuplify, tuplify2, lVarExp) where

import Prelude hiding (exp)
import Data.Set (Set)
import "template-haskell" Language.Haskell.TH (Name, Stmt(..), Exp(..))

tuplify :: Name -> Exp -> Exp -> Exp
tuplify s lhs r = TupE [lhs, VarE s, r]

tuplify2 :: Exp -> Exp -> Exp
tuplify2 m ar = TupE [m, ar]

lVarExp :: (Name -> Set Name -> Exp) -> Set Name -> Exp -> Exp
lVarExp f nms (VarE nm) = f nm nms
lVarExp f nms (AppE e1 e2) = AppE (lVarExp f nms e1) (lVarExp f nms e2)
lVarExp f nms (TupE exps) = TupE (map (lVarExp f nms) exps)
lVarExp f nms (ListE exps) = ListE (map (lVarExp f nms) exps)
lVarExp f nms (UInfixE e1 e2 e3) = UInfixE (lVarExp f nms e1) (lVarExp f nms e2) (lVarExp f nms e3)
lVarExp f nms (ParensE e) = ParensE (lVarExp f nms e)
lVarExp f nms (LamE pats e) = LamE pats (lVarExp f nms e)
lVarExp f nms (CompE stmts) = CompE (map (tStmt nms) stmts) where
    tStmt nms' (BindS p e) = BindS p (lVarExp f nms' e)
    tStmt nms' (NoBindS e) = NoBindS (lVarExp f nms' e)
    tStmt _ LetS{} = error "Unexpected LetS"
    tStmt _ ParS{} = error "Unexpected ParS"
lVarExp f nms (InfixE me1 e me2) =
    InfixE (fmap (lVarExp f nms) me1) (lVarExp f nms e) (fmap (lVarExp f nms) me2)
lVarExp _ _ (LitE lit) = LitE lit
lVarExp _ _ (ConE nm) = ConE nm
lVarExp f nms (RecConE nm fexps) = RecConE nm (map (tFExp nms) fexps) where
    tFExp nms' (nm', exp) = (nm', lVarExp f nms' exp)
lVarExp f nms (CondE e1 e2 e3) = CondE (lVarExp f nms e1) (lVarExp f nms e2) (lVarExp f nms e3)
lVarExp _ _nms _ = undefined
