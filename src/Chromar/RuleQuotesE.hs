module Chromar.RuleQuotesE where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec
import Data.List
import Chromar.MRuleParser
import Chromar.MAttrs

type FieldProd = (FieldPat, [Exp], Set Name)

data SRule = SRule
    { lexps :: [Exp]
    , rexps :: [Exp]
    , mults :: [Exp]
    , srate :: Exp
    , cond :: Exp
    , decs :: [Dec]  
    } deriving (Show)


{- haskellify the Chromar rule parts
   left agent exprs become Haskell record exprs etc.
-}
translate :: ARule Exp -> SRule
translate = undefined

rule :: QuasiQuoter
rule =
    QuasiQuoter
    { quoteExp = ruleQuoter
    , quotePat = undefined
    , quoteDec = undefined
    , quoteType = undefined
    }

--- pure action
tFieldPat :: Set Name -> Name -> FieldExp -> FieldProd
tFieldPat names freshNm (nm, VarE pnm) =
    if Set.member pnm names
        then ( (nm, VarP freshNm)
             , [UInfixE (VarE freshNm) (VarE $ mkName "==") (VarE pnm)]
             , Set.empty)
        else ((nm, VarP pnm), [], Set.fromList [pnm])
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
mkPatStmt nm fpats = BindS pat (VarE $ mkName "s")
  where
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
tAgentPat sn (RecConE nm fexps) = mkAgentStmts nm qexps
  where
    qfps = mapM (qtFieldPat sn) fexps
    qexps = mkAgentExps qfps
tAgentPat _ _ = error "expected records"

mkLhsStmts :: Set Name -> [Stmt] -> [Exp] -> Q [Stmt]
mkLhsStmts sn allStmts [] = return allStmts
mkLhsStmts sn allStmts (exp:exps) = do
    (stmts, sn') <- tAgentPat sn exp
    mkLhsStmts (Set.union sn sn') (allStmts ++ stmts) exps

mkLhs :: [Exp] -> Q [Stmt]
mkLhs = mkLhsStmts Set.empty []

isEr :: Info -> Bool
isEr (VarI _ t _ _) = isErT t
  where
    isErT (ConT nm) = "ErF" `isSuffixOf` (show nm)
    isErT (ForallT _ _ tp) = isErT tp
    isErT (AppT tp _) = isErT tp
    isErT _ = False
isEr _ = False

mkErFApp :: Name -> Exp
mkErFApp nm =
    ParensE
        (AppE
             (AppE (AppE (VarE $ mkName "at") (VarE nm)) (VarE $ mkName "s"))
             (VarE $ mkName "t"))

tStmt :: Stmt -> Q Stmt
tStmt (BindS p e) = do
    te <- tExp e
    return $ BindS p te
tStmt (NoBindS e) = do
    te <- tExp e
    return $ NoBindS te

tMExp :: Maybe Exp -> Q (Maybe Exp)
tMExp (Just e) = do
    te <- tExp e
    return (Just te)
tMExp Nothing = return Nothing

tName :: Maybe Name -> Exp -> Q Exp
tName (Just nm) exp = do
    info <- reify nm
    if isEr info
        then return $ mkErFApp nm
        else return exp
tName Nothing exp = return exp

--- there's probably a better way of doing this
tExp :: Exp -> Q Exp
tExp var@(VarE nm) = do
    mnm <- lookupValueName (show nm)
    tName mnm var
tExp (AppE e1 e2) = do
    te1 <- tExp e1
    te2 <- tExp e2
    return $ AppE te1 te2
tExp (TupE exps) = do
    texps <- mapM tExp exps
    return $ TupE texps
tExp (ListE exps) = do
    texps <- mapM tExp exps
    return $ ListE texps
tExp (UInfixE e1 e2 e3) = do
    te1 <- tExp e1
    te2 <- tExp e2
    te3 <- tExp e3
    return $ UInfixE te1 te2 te3
tExp (ParensE e) = do
    te <- tExp e
    return $ ParensE te
tExp (LamE pats e) = do
    te <- tExp e
    return $ LamE pats te
tExp (CompE stmts) = do
    tstmts <- mapM tStmt stmts
    return $ CompE tstmts
tExp (InfixE me1 e me2) = do
    tme1 <- tMExp me1
    te <- tExp e
    tme2 <- tMExp me2
    return $ InfixE tme1 te tme2
tExp (LitE lit) = return $ LitE lit
tExp (ConE nm) = return $ ConE nm
tExp (RecConE nm fexps) = do
    tfexps <- mapM tFExp fexps
    return $ RecConE nm tfexps
tExp _ = undefined

tFExp :: FieldExp -> Q FieldExp
tFExp (nm, exp) = do
    te <- tExp exp
    return (nm, te)

tBody :: Body -> Q Body
tBody (NormalB exp) = do
  te <- tExp exp
  return (NormalB te)
tBody _ = error "expected NormalB constr"

tDec :: Dec -> Q Dec
tDec (ValD p bd xs) = do
  tbd <- tBody bd
  return (ValD p tbd xs)
tDec _ = error "expected ValD constr"

tuplify :: Name -> Exp -> Exp -> Exp
tuplify s lhs r = TupE [lhs, VarE s, r]

tuplify2 :: Exp -> Exp -> Exp
tuplify2 m ar = TupE [m, ar]

mkActExp :: Name -> Exp -> Exp -> Exp
mkActExp s lhs r = AppE (VarE $ mkName "fullRate") args
  where
    args = tuplify s lhs r

mkReturnStmt :: Exp -> Stmt
mkReturnStmt = NoBindS

mkRxnExp :: Name -> SRule -> Exp
mkRxnExp s r = RecConE (mkName "Rxn") fields
  where
    lhsSym = mkName "lhs"
    rhsSym = mkName "rhs"
    rateSym = mkName "rate"
    actSym = mkName "act"
    mrexps =
        AppE
            (VarE $ mkName "nrepl")
            (tuplify2 (ListE $ mults r) (ListE $ rexps r))
    lexps' = AppE (VarE $ mkName "ms") (ListE $ lexps r)
    rexps' = AppE (VarE $ mkName "ms") (ParensE mrexps)
    rateExp = srate r
    actExp = mkActExp s lexps' (srate r)
    fields =
        [ (lhsSym, lexps')
        , (rhsSym, rexps')
        , (rateSym, rateExp)
        , (actSym, actExp)
        ]

mkCompStmts :: Name -> SRule -> Q [Stmt]
mkCompStmts s r = do
    let rxnExp = mkRxnExp s r
    let retStmt = mkReturnStmt rxnExp
    let guardStmt = NoBindS (cond r)
    let letStmt = LetS (decs r)
    patStmts <- mkLhs (lexps r)
    return $ patStmts ++ [letStmt, guardStmt, retStmt]

ruleQuoter' :: SRule -> Q Exp
ruleQuoter' r = do
    state <- newName "s"
    time <- newName "t"
    stmts <- mkCompStmts state r
    return $ LamE [VarP state, VarP time] (CompE stmts)

fluentTransform :: SRule -> Q SRule
fluentTransform SRule {lexps = les
                      ,rexps = res
                      ,mults = m         
                      ,srate = r
                      ,cond = c
                      ,decs = ds} = do
    re <- tExp r
    ce <- tExp c
    tres <- mapM tExp res
    tds <- mapM tDec ds
    return
        SRule
        { lexps = les
        , rexps = tres
        , mults = m          
        , srate = re
        , cond = ce
        , decs = tds         
        }

ruleQuoter :: String -> Q Exp
ruleQuoter s =
    case parse parseRule "" s of
        Left err -> error (show err)
        Right r -> do
            sr <- fluentTransform r
            sr' <- fillAttrs sr
            ruleQuoter' sr'

ruleQuoter'' :: String -> Q Exp
ruleQuoter'' s = do
  info <- reify (mkName $ s)
  stringE (show info)