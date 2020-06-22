module Chromar.RuleQuotesE where

import Prelude hiding (exp)
import Data.Set (Set)
import Data.Bifunctor (bimap)
import qualified Data.Set as Set
import Language.Haskell.TH
    ( Q, Name, Stmt(..), Lit(..)
    , Pat(..), FieldPat
    , Exp(..), FieldExp
    , newName, mkName, reify, stringE
    )
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (showName)
import Text.ParserCombinators.Parsec (parse)
import Chromar.MRuleParser (LAgent(..), RAgent(..), ARule(..), SRule(..), parseRule)
import Chromar.MAttrs (fillAttrs)
import qualified Chromar.RExprs as RE
import Internal.RuleQuotes as RE

type FieldProd = (FieldPat, [Exp], Set Name)

tLAgent :: LAgent -> Exp
tLAgent (LAgent nm attrs) =
    RecConE (mkName nm) (map (\(aNm, v) -> (mkName aNm, VarE $ mkName v)) attrs)

tRAgent :: RAgent Exp -> Exp
tRAgent (RAgent nm attrs) =
    RecConE
        (mkName nm)
        (bimap mkName (RE.mkErApp' . RE.quoteEr) <$> attrs)

tRateE :: RE.SEr Exp -> Exp
tRateE = RE.mkErApp' . RE.quoteEr

tCondE :: RE.SEr Exp -> Exp
tCondE = RE.mkErApp' . RE.quoteEr

{- haskellify the Chromar rule parts
   left agent exprs become Haskell record exprs etc.
-}
tRule :: ARule Exp -> SRule
tRule Rule{rlhs = ls, rrhs = rs, mults = ms, rexpr = r, cexpr = c} =
    SRule
        { lexps = map tLAgent ls
        , rexps = map tRAgent rs
        , multExps = ms
        , srate = tRateE r
        , cond = tCondE c
        }

--- pure action
tFieldPat :: Set Name -> Name -> FieldExp -> FieldProd
tFieldPat names freshNm (nm, VarE pnm) =
    if Set.member pnm names
        then ( (nm, VarP freshNm)
             , [UInfixE (VarE freshNm) (VarE $ mkName "==") (VarE pnm)]
             , Set.empty
             )
        else ((nm, VarP pnm), [], Set.fromList [pnm])
tFieldPat _name freshNm (nm, exp) =
    ( (nm, VarP freshNm)
    , [UInfixE (VarE freshNm) (VarE $ mkName "==") exp]
    , Set.empty
    )

--- monadic action
qtFieldPat :: Set Name -> FieldExp -> Q FieldProd
qtFieldPat names fexp@(nm, _exp) = do
    fn <- newName (showName nm)
    return $ tFieldPat names fn fexp

mkGuardExp :: [[Exp]] -> Exp
mkGuardExp expss = AppE andFunc (ListE exps)
  where
    andFunc = VarE (mkName "and")
    exps = concat expss

mkAgentExps :: Q [FieldProd] -> Q ([FieldPat], Exp, Set Name)
mkAgentExps qfps = do
    fps <- qfps
    let (fpats, exprss, sets) = unzip3 fps
    let guardExp = mkGuardExp exprss
    let sn = Set.unions sets
    return (fpats, guardExp, sn)

mkPatStmt :: Name -> [FieldPat] -> Stmt
mkPatStmt nm fpats = BindS pat (VarE $ mkName "s") where
    pat = TupP [RecP nm fpats, WildP]

mkGuardStmt :: Exp -> Stmt
mkGuardStmt = NoBindS

mkAgentStmts :: Name -> Q ([FieldPat], Exp, Set Name) -> Q ([Stmt], Set Name)
mkAgentStmts nm qexps = do
    (fpats, gExp, sn) <- qexps
    let patStmt = mkPatStmt nm fpats
    let guardStmt = mkGuardStmt gExp
    return ([patStmt, guardStmt], sn)

tAgentPat :: Set Name -> Exp -> Q ([Stmt], Set Name)
tAgentPat sn (RecConE nm fexps) = mkAgentStmts nm qexps where
    qfps = mapM (qtFieldPat sn) fexps
    qexps = mkAgentExps qfps
tAgentPat _ _ = error "expected records"

mkLhsStmts :: Set Name -> [Stmt] -> [Exp] -> Q [Stmt]
mkLhsStmts _ allStmts [] = return allStmts
mkLhsStmts sn allStmts (exp:exps) = do
    (stmts, sn') <- tAgentPat sn exp
    mkLhsStmts (Set.union sn sn') (allStmts ++ stmts) exps

mkLhs :: [Exp] -> Q [Stmt]
mkLhs = mkLhsStmts Set.empty []

mkErFApp :: Name -> Exp
mkErFApp nm =
    ParensE
        (AppE
             (AppE (AppE (VarE $ mkName "at") (VarE nm)) (VarE $ mkName "s"))
             (VarE $ mkName "t"))

mkActExp :: Name -> Exp -> Exp -> Exp
mkActExp s lhs r = AppE (VarE $ mkName "fullRate") args
  where
    args = tuplify s lhs r

mkReturnStmt :: Exp -> Stmt
mkReturnStmt = NoBindS

mkFApp' :: Exp -> [Exp] -> Exp
mkFApp' _ [] = undefined
mkFApp' f (e:exps) = foldr (flip AppE) (AppE f e) (reverse exps)

mkRxnExp :: Name -> SRule -> Exp
mkRxnExp s r = RecConE (mkName "Rxn") fields where
    lhsSym = mkName "lhs"
    rhsSym = mkName "rhs"
    rateSym = mkName "rate"
    mrexps =
        AppE
            (VarE $ mkName "nrepl")
            (tuplify2 (ListE $ multExps r) (ListE $ rexps r))
    lexps' = AppE (VarE $ mkName "ms") (ListE $ lexps r)
    rexps' = AppE (VarE $ mkName "ms") (ParensE mrexps)
    rateExp = mkActExp s lexps' (srate r)
    fields = [(lhsSym, lexps'), (rhsSym, rexps'), (rateSym, rateExp)]

mkCompStmts :: Name -> SRule -> Q [Stmt]
mkCompStmts s r = do
    let rxnExp = mkRxnExp s r
    let retStmt = mkReturnStmt rxnExp
    let guardStmt = NoBindS (cond r)
    let lexps' = AppE (VarE $ mkName "ms") (ListE $ lexps r)
    let actStmt =
            NoBindS
                (mkFApp'
                     (VarE $ mkName ">")
                     [mkActExp s lexps' (srate r), LitE (IntegerL 0)])
    patStmts <- mkLhs (lexps r)
    return $ patStmts ++ [guardStmt, actStmt, retStmt]

ruleQuoter' :: SRule -> Q Exp
ruleQuoter' r = do
    state <- newName "s"
    time <- newName "t"
    stmts <- mkCompStmts state r
    return $ LamE [VarP state, VarP time] (CompE stmts)

ruleQuoter :: String -> Q Exp
ruleQuoter s =
    case parse parseRule "" s of
        Left err -> error (show err)
        Right r -> do
            let sr = tRule r
            sr' <- fillAttrs sr
            ruleQuoter' sr'

rule :: QuasiQuoter
rule =
    QuasiQuoter
        { quoteExp = ruleQuoter
        , quotePat = undefined
        , quoteDec = undefined
        , quoteType = undefined
        }

--- for testing
ruleQuoter'' :: String -> Q Exp
ruleQuoter'' s = do
    info <- reify $ mkName s
    stringE (show info)
