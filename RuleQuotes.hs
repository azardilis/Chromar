module RuleQuotes where

import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec
import RuleParser

rule :: QuasiQuoter
rule = QuasiQuoter { quoteExp  = ruleQuoter,
                     quotePat  = undefined,
                     quoteDec  = undefined,
                     quoteType = undefined }


mkPatStmts :: Name -> [Pat] -> [Stmt]
mkPatStmts s pats = [BindS (tpat pat) (VarE s) | pat <- pats] where
  tpat p = TupP [p, WildP]


mkRxnExp :: SRule -> Exp
mkRxnExp r = RecConE (mkName "Rxn") fields where
  lhsSym  = mkName "lhs"
  rhsSym  = mkName "rhs"
  rateSym = mkName "rate"
  lexps'  = AppE (VarE $ mkName "ms") (ListE $ lexps r)
  rexps'  = AppE (VarE $ mkName "ms") (ListE $ rexps r)
  fields  = [ (lhsSym , lexps'),
              (rhsSym , rexps'),
              (rateSym, rate r)
            ]


mkReturnStmt :: Exp -> Stmt
mkReturnStmt = NoBindS


mkGuardStmt :: Exp -> Stmt
mkGuardStmt = NoBindS


mkCompStmts :: Name -> SRule -> [Stmt]
mkCompStmts s r = patStmts ++ [guardStmt, retStmt] where
  rxnExp    = mkRxnExp r
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
