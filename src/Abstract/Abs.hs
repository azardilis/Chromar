{-# LANGUAGE DeriveGeneric #-}

module Abs where

import Data.Bifunctor
import GHC.Generics
import Data.Sexp
import Language.Sexp.Printer
import Data.Ratio
import Data.Word8
import Language.Haskell.Meta.Parse
import Chromar.MRuleParser
import Language.Haskell.Meta.Syntax.Translate
import Text.Parsec
import Language.Haskell.Exts (parseFile)
import Language.Haskell.Exts.Parser (ParseResult(..))
import qualified Language.Haskell.Exts.Syntax as E
import qualified Language.Haskell.TH.Syntax as H

type Nm = String

type AttrName = String

type Var = String

type Multiset a = [(a, Int)]

data AgentType t =
    AgentType Nm
              [(AttrName, t)] deriving (Generic, Show)
                                       
instance (Sexpable t) => Sexpable (AgentType t)

instance Functor AgentType where
    fmap f (AgentType nm attrs) =
        AgentType
            nm
            [ (attrNm, f t)
            | (attrNm, t) <- attrs ]

data RAgent e =
    RAgent Nm
           [(AttrName, e)] deriving (Generic, Show)

instance (Sexpable e) => Sexpable (RAgent e) 

instance Functor RAgent where
    fmap f (RAgent nm attrs) =
        RAgent
            nm
            [ (attr, f e)
            | (attr, e) <- attrs ]

data LAgent =
    LAgent Nm
           [(AttrName, Var)] deriving (Generic, Show)

instance Sexpable LAgent

data ARule e = Rule
    { lhs :: [LAgent]
    , rhs :: [RAgent e]
    , rexpr :: e
    , cexpr :: e
    } deriving (Generic, Show)

instance (Sexpable e) => Sexpable (ARule e)

instance Functor ARule where
    fmap f (Rule {lhs = lhs
                 ,rhs = rhs
                 ,rexpr = re
                 ,cexpr = ce}) =
        Rule
        { lhs = lhs
        , rhs =
            [ fmap f rhsAt
            | rhsAt <- rhs ]
        , rexpr = f re
        , cexpr = f ce
        }

data Chromar e t = Chromar
    { agentDecls :: [AgentType t]
    , iState :: Multiset (RAgent e)
    , rules :: [ARule e]
    } deriving (Generic, Show)

instance (Sexpable e, Sexpable t) => Sexpable (Chromar e t)

instance Bifunctor Chromar where
    bimap ef tf (Chromar {agentDecls = adecls
                         ,iState = ist
                         ,rules = rs}) =
        Chromar
        { agentDecls =
            [ fmap tf ad
            | ad <- adecls ]
        , iState =
            [ (fmap ef el, n)
            | (el, n) <- ist ]
        , rules =
            [ fmap ef r
            | r <- rs ]
        }
    first ef (Chromar {agentDecls = adecls
                      ,iState = ist
                      ,rules = rs}) =
        Chromar
        { agentDecls = adecls
        , iState =
            [ (fmap ef el, n)
            | (el, n) <- ist ]
        , rules =
            [ fmap ef r
            | r <- rs ]
        }
    second tf (Chromar {agentDecls = adecls
                       ,iState = ist
                       ,rules = rs}) =
        Chromar
        { agentDecls =
            [ fmap tf ad
            | ad <- adecls ]
        , iState = ist
        , rules = rs
        }
                        
{-
then for every change in the 'inside language' we do bimap ...
for every change in the representation we need (le'ts take xml as an example)

instance (ToXML e, ToXML t) => ToXML (Chromar e t) where
   toXML (Chromar{..}) = ...
-} 


{-
Example translation where e=H.Exp and t= H.Type
-}

instance Sexpable H.TyVarBndr
instance Sexpable H.Stmt
instance Sexpable H.Type
instance Sexpable H.Dec
instance Sexpable H.Body
instance Sexpable H.Guard
instance Sexpable H.TyLit
instance Sexpable H.Clause
instance Sexpable H.Match
instance Sexpable H.FunDep
instance Sexpable H.Pat
instance Sexpable H.TySynEqn
instance Sexpable H.Name
instance Sexpable H.Role
instance Sexpable H.OccName
instance Sexpable H.Range
instance Sexpable H.Pragma
instance Sexpable H.NameFlavour
instance Sexpable H.Lit
instance Sexpable H.Foreign
instance Sexpable H.RuleMatch
instance Sexpable H.PkgName
instance Sexpable H.Fixity
instance Sexpable H.RuleBndr
instance Sexpable H.NameSpace
instance Sexpable H.Safety
instance Sexpable H.FamFlavour
instance Sexpable H.Phases
instance Sexpable H.ModName
instance Sexpable H.Callconv
instance Sexpable H.FixityDirection
instance Sexpable H.Con
instance Sexpable H.Inline
instance Sexpable H.AnnTarget
instance Sexpable H.Strict
instance Sexpable H.Exp


instance (Integral a, Sexpable a) => Sexpable (Ratio a) where
  toSexp r = List [toSexp $ numerator r, toSexp $ denominator r]

  fromSexp s = undefined


instance Sexpable Word8 where
  toSexp = undefined
  fromSexp = undefined

parseE :: String -> H.Exp
parseE s = case parseExp s of
  Left s -> error s
  Right e -> e

parseT :: String -> H.Type
parseT t = case parseType t of
  Left s -> error s
  Right t -> t

----create a Chromar(H.Exp, H.T)
arule = Rule { lhs = [LAgent "A" [("x", "x")]],
               rhs = [RAgent "A" [("x", parseE "x+1")]],
               rexpr = parseE "x",
               cexpr = parseE "True" }

adecl = AgentType "A" [("x", parseT "Int")]

m = Chromar { agentDecls = [adecl],
              iState = [(RAgent "A" [("x", parseE "1")], 1)],
              rules = [arule] }

{-
a filepath containing a Chromar(H.E, H.T) model in Haskell surface syntax to an abstract representation
-}
fromMod :: FilePath -> IO (Chromar H.Exp H.Type)
fromMod fn = do
  res <- parseFile fn
  case res of
    (ParseOk m) -> return $ getAbsChromar m
    (ParseFailed l s) -> error ""

toSexpMod :: FilePath -> FilePath -> IO ()
toSexpMod fin fout = undefined

getAbsChromar :: E.Module -> Chromar H.Exp H.Type
getAbsChromar (E.Module _ _ _ _ _ _ decs) =
    Chromar
    { agentDecls = concatMap getATypes decs
    , iState = [] -- to implement (see getIState)
    , rules = concatMap getRules decs
    }

getRules :: E.Decl -> [ARule H.Exp]
getRules (E.PatBind _ (E.PVar (E.Ident nm)) (E.UnGuardedRhs e) _) = goRule e
  where
    goRule (E.QuasiQuote quoter r)
      | quoter == "rule" = [parseARule r]
      | otherwise = []
    goRule _ = []
getRules _ = []
                                              
getATypes :: E.Decl -> [AgentType H.Type]
getATypes (E.DataDecl _ E.DataType _ (E.Ident nm) _ constrs _)
  | nm == "Agent" = map getAT constrs
  | otherwise = []                  
getATypes _ = []

getAT :: E.QualConDecl -> AgentType H.Type
getAT (E.QualConDecl _ _ _ (E.RecDecl (E.Ident nm) attrs)) = AgentType nm (map goAttr attrs)
  where
    goAttr ([E.Ident nm], t) = (nm, toType t)
    goAttr _ = undefined
getAT _ = undefined

getIState :: E.Decl -> [Multiset (RAgent H.Exp)]
getIState = undefined

toAbsLAgent :: H.Exp -> [LAgent]
toAbsLAgent (H.RecConE nm fexps) = [LAgent (show nm) (concatMap goFExp fexps)]
  where
    goFExp (fnm, H.VarE v) = [(show fnm, show v)]
    goFExp _ = []
toAbsLAgent _ = []

toAbsRAgent :: H.Exp -> [RAgent H.Exp]
toAbsRAgent (H.RecConE nm fexps) = [RAgent (show nm) (map goFExp fexps)]
  where
    goFExp (fnm, e) = (show fnm, e)
toAbsRAgent _ = []

mkAbsRule :: SRule -> ARule H.Exp
mkAbsRule (SRule {lexps = les
                 ,rexps = res
                 ,mults = _
                 ,srate = re
                 ,cond = ce
                 ,decs = _}) =
    Rule
    { lhs = concatMap toAbsLAgent les
    , rhs = concatMap toAbsRAgent res
    , rexpr = re
    , cexpr = ce
    }

parseARule :: String -> ARule H.Exp
parseARule s = case parse parseRule "rule" s of
  (Left err) -> error (show err)
  (Right sr) -> mkAbsRule sr

