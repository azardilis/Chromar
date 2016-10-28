module Chromar.RuleQuotes where

import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec
import Chromar.RuleParser

rule :: QuasiQuoter
rule = QuasiQuoter { quoteExp  = ruleQuoter,
                     quotePat  = undefined,
                     quoteDec  = undefined,
                     quoteType = undefined }


mkPatStmts :: Name -> [Pat] -> [Stmt]
mkPatStmts s pats = [BindS (tpat pat) (VarE s) | pat <- pats] where
  tpat p = TupP [p, WildP]


tuplify :: Name -> Exp -> Exp -> Exp
tuplify s lhs r = TupE [lhs, VarE s, r]


mkRateExp :: Name -> Exp -> Exp -> Exp
mkRateExp s lhs r = AppE (VarE $ mkName "fullRate") args where
  args = tuplify s lhs r


mkRxnExp :: Name -> SRule -> Exp
mkRxnExp s r = RecConE (mkName "Rxn") fields where
  lhsSym  = mkName "lhs"
  rhsSym  = mkName "rhs"
  rateSym = mkName "rate"
  lexps'  = AppE (VarE $ mkName "ms") (ListE $ lexps r)
  rexps'  = AppE (VarE $ mkName "ms") (ListE $ rexps r)
  rateExp = mkRateExp s lexps' (srate r) 
  fields  = [ (lhsSym , lexps'),
              (rhsSym , rexps'),
              (rateSym, rateExp)
            ]


mkReturnStmt :: Exp -> Stmt
mkReturnStmt = NoBindS


mkGuardStmt :: Exp -> Stmt
mkGuardStmt = NoBindS


mkCompStmts :: Name -> SRule -> [Stmt]
mkCompStmts s r = patStmts ++ [guardStmt, retStmt] where
  rxnExp    = mkRxnExp s r
  patStmts  = mkPatStmts s (lpats r)
  retStmt   = mkReturnStmt rxnExp
  guardStmt = mkGuardStmt (cond r)
  

ruleQuoter' :: SRule -> Q Exp
ruleQuoter' r = do
  state <- newName "s"
  return $ LamE [VarP state] (CompE (mkCompStmts state r))


ruleQuoter :: String -> Q Exp
ruleQuoter s = case parse parseRule "" s of
  Left err  -> error (show err)
  Right r   -> ruleQuoter' r
