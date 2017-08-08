module Abs where

import Data.Bifunctor

type Multiset a = [(a, Int)]

type TEnv t = [(Nm, t)]

type Nm = String

type AttrName = String

type Var = String

data AgentType t =
    AgentType Nm
              [(AttrName, t)]

instance Functor AgentType where
    fmap f (AgentType nm attrs) =
        AgentType
            nm
            [ (attrNm, f t)
            | (attrNm, t) <- attrs ]

data RAgent e =
    RAgent Nm
           [(AttrName, e)]

instance Functor RAgent where
    fmap f (RAgent nm attrs) =
        RAgent
            nm
            [ (attr, f e)
            | (attr, e) <- attrs ]

data LAgent =
    LAgent Nm
           [(AttrName, Var)]

data ARule e = Rule
    { lhs :: [LAgent]
    , rhs :: [RAgent e]
    , rexpr :: e
    , cexpr :: e
    }

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
        , cexpr = f re
        }

data Chromar e t = Chromar
    { agentDecls :: [AgentType t]
    , iState :: Multiset (RAgent e)
    , rules :: [ARule e]
    }


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

