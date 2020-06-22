{-# LANGUAGE PackageImports #-}

module Chromar.MAttrs where

import Chromar.MRuleParser
import qualified Data.Map as M
import qualified Data.Set as S
import "template-haskell" Language.Haskell.TH

data AgentType = AgentT Nm (S.Set Nm) deriving (Show)

getN :: AgentType -> Nm
getN (AgentT nm _) = nm

getIntf :: AgentType -> S.Set Nm
getIntf (AgentT _ iface) = iface

intf :: Exp -> [FieldExp]
intf (RecConE _ fexps) = fexps
intf _ = error "Expected records"

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

getType :: Con -> AgentType
getType (RecC nm ifce) = AgentT (nameBase nm) (S.fromList fNames) where
    fNames = map (nameBase . fst3) ifce
getType _ = error "Expected records"

extractIntf :: Info -> [AgentType]
extractIntf (TyConI (DataD _ _ _ _ cons _)) = map getType cons
extractIntf _ = error "Expected type constructor"

createMFExp :: Nm -> Q FieldExp
createMFExp nm = do
    varNm <- newName nm
    return (mkName nm, VarE varNm)

fillPat :: AgentType -> Exp -> Q Exp
fillPat typ (RecConE nm fexps) = do
    let fIntf = getIntf typ
    let pIntf = S.fromList $ map (nameBase . fst) fexps
    let mAttrs = S.difference fIntf pIntf
    mFExps <- mapM createMFExp (S.toList mAttrs)
    return $ RecConE nm (fexps ++ mFExps)
fillPat _ _ = error "Expected record patterns"

lookupType :: [AgentType] -> Nm -> AgentType
lookupType ats nm = head $ filter (\at -> getN at == nm) ats

fPat :: [AgentType] -> Exp -> Q Exp
fPat ats e@(RecConE nm _) = fillPat at e where
    at = lookupType ats (nameBase nm)
fPat _ _ = error "Expected record patterns"

fRExp :: Exp -> Exp -> Exp
fRExp lexp (RecConE nm rIntf) = RecConE nm (M.toList pIntf') where
    fIntf = M.fromList (intf lexp)
    pIntf = M.fromList rIntf
    pIntf' = M.union pIntf (M.difference fIntf pIntf)
fRExp _ _ = error "Expected records"

sameType :: Exp -> Exp -> Bool
sameType (RecConE nm _) (RecConE nm' _) = nm == nm'
sameType _ _ = error "Expected records"

tRExp :: Exp -> Exp -> Exp
tRExp l r
    | sameType l r = fRExp l r
    | otherwise = r

lZipWith :: (a -> b -> b) -> [a] -> [b] -> [b]
lZipWith _ _ls [] = []
lZipWith _ [] rs = rs
lZipWith f (l:ls) (r:rs) = f l r : lZipWith f ls rs

fillAttrs :: SRule -> Q SRule
fillAttrs
    SRule
        { lexps = les
        , rexps = res
        , multExps = m
        , srate = r
        , cond = c
        } = do
    info <- reify (mkName "Agent")
    let aTyps = extractIntf info
    les' <- mapM (fPat aTyps) les
    let res' = lZipWith tRExp les' res
    return
        SRule
            { lexps = les'
            , rexps = res'
            , multExps = m
            , srate = r
            , cond = c
            }
