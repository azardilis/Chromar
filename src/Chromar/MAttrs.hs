module Chromar.MAttrs where

import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Chromar.RuleParser


type Nm = String

data AgentType = AgentT Nm (S.Set Nm) deriving (Show)


getN :: AgentType -> Nm
getN (AgentT nm _) = nm


getIntf :: AgentType -> S.Set Nm
getIntf (AgentT _ intf) = intf


intf :: Exp -> [FieldExp]
intf (RecConE _ fexps) = fexps
intf _ = error "Expected records"


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x


getType :: Con -> AgentType
getType (RecC nm intf) = AgentT (nameBase nm) (S.fromList fNames)
  where
    fNames = map (nameBase . fst3) intf
getType _ = error "Expected records"

  
extractIntf :: Info -> [AgentType]
extractIntf (TyConI (DataD _ _ _ cons _)) = map getType cons
extractIntf _ = error "Expected records"


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


lookupType :: [AgentType] -> Nm -> AgentType
lookupType ats nm = head $ filter (\at -> getN at == nm) ats


fPat :: [AgentType] -> Exp -> Q Exp
fPat ats e@(RecConE nm _) = fillPat at e
  where
    at = lookupType ats (nameBase nm)


fRExp :: Exp -> Exp -> Exp
fRExp lexp (RecConE nm rIntf) = RecConE nm (M.toList pIntf')
  where
    fIntf = M.fromList (intf lexp)
    pIntf = M.fromList rIntf
    pIntf' = M.union pIntf (M.difference fIntf pIntf)


fillAttrs :: SRule -> Q SRule
fillAttrs SRule { lexps = les
                , rexps = res
                , srate = r
                , cond = c
                } = do
  info <- reify (mkName "Agent")
  let aTyps = extractIntf info
  les' <- mapM (fPat aTyps) les
  let res' = [fRExp l r | (l, r) <- zip les' res]
  return SRule {lexps = les', rexps = res', srate = r, cond = c}
