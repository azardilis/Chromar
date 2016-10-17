module Ext where

import Language.Haskell.TH
import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec
import Language.Haskell.Exts.Pretty
import RuleParser


type FieldProd = (FieldPat, [Exp], Set Name)


rule :: QuasiQuoter
rule = QuasiQuoter { quoteExp  = ruleQuoter,
                     quotePat  = undefined,
                     quoteDec  = undefined,
                     quoteType = undefined }


ruleQuoter :: String -> Q Exp
ruleQuoter s = case parse parseRuleSide "" s of
  Left err  -> error (show err)
  Right r   -> foo' r


--- pure action
tFieldPat :: Set Name -> Name -> FieldExp -> FieldProd
tFieldPat names freshNm (nm, VarE pnm) =
  case (Set.member pnm names) of
    False -> ((nm, VarP pnm), [], Set.fromList [pnm])
    True ->
      ( (nm, VarP freshNm)
      , [UInfixE (VarE freshNm) (VarE $ mkName "==") (VarE pnm)]
      , Set.empty )
tFieldPat name freshNm (nm, exp) =
  ( (nm, VarP freshNm)
  , [UInfixE (VarE freshNm) (VarE $ mkName "==") exp]
  , Set.empty)


--- monadic action
qtFieldPat :: Set Name -> FieldExp -> Q FieldProd
qtFieldPat names fexp@(nm, exp) = do
  fn <- newName (showName nm)
  return $ tFieldPat names fn fexp


mkGuardExp :: [[Exp]] -> Exp
mkGuardExp expss = AppE andFunc (ListE exps) where
  andFunc = VarE (mkName "and")
  exps    = concat expss


mkAgentExps :: Q [FieldProd] -> Q ([FieldPat], Exp, Set Name)
mkAgentExps qfps = do
  fps <- qfps
  let (fpats, exprss, sets) = unzip3 fps
  let guardExp = mkGuardExp exprss
  let sn = Set.unions sets
  return $ (fpats, guardExp, sn)


mkPatStmt :: Name -> [FieldPat] -> Stmt
mkPatStmt nm fpats = BindS pat (VarE $ mkName "s") where
  pat = RecP nm fpats


mkGuardStmt :: Exp -> Stmt
mkGuardStmt = NoBindS 


mkAgentStmts :: Name -> Q ([FieldPat], Exp, Set Name) -> Q ([Stmt], Set Name)
mkAgentStmts nm qexps = do
  (fpats, gExp, sn) <- qexps
  let patStmt = mkPatStmt nm fpats
  let guardStmt = mkGuardStmt gExp
  return $ ([patStmt, guardStmt], sn)


tAgentPat :: Set Name -> Exp -> Q ([Stmt], Set Name)
tAgentPat sn (RecConE nm fexps) = mkAgentStmts nm qexps where
  qfps  = mapM (qtFieldPat sn) fexps
  qexps = mkAgentExps qfps


mkLhsStmts :: Set Name -> [Stmt] -> [Exp] -> Q [Stmt]
mkLhsStmts sn allStmts [] = return $ allStmts
mkLhsStmts sn allStmts (exp:exps) = do
  (stmts, sn') <- tAgentPat sn exp
  mkLhsStmts (Set.union sn sn') (allStmts ++ stmts) exps


mkLhs :: [Exp] -> Q [Stmt]
mkLhs exps = mkLhsStmts Set.empty [] exps


mkLhsExp :: Q Exp
mkLhsExp = do
  state <- newName "s"
  return $
    LamE
      [VarP state]
      (UInfixE (VarE $ mkName "s") (VarE $ mkName "==") (LitE (IntegerL 1)))


foo :: Set Name -> Exp -> Q Exp
foo sn exp = do
  (stmts, sn) <- tAgentPat sn exp
  stringE (show stmts)


foo' :: [String] -> Q Exp
foo' rs = do
  let exprs = createExps rs
  stmts <- mkLhs exprs
  stringE (show stmts)
