{-# LANGUAGE PackageImports #-}

module Chromar.Rule.Syntax
    ( SRule(..), ARule(..)
    , LAgent(..), RAgent(..)
    , Nm, Var, AttrName
    ) where

import "template-haskell" Language.Haskell.TH.Syntax (Exp(..))

import Chromar.Enriched.Syntax (SEr)

type Nm = String
type Var = String
type AttrName = String

data RAgent e = RAgent Nm [(AttrName, SEr e)] deriving (Show)
data LAgent = LAgent Nm [(AttrName, Var)] deriving (Show)

data ARule e =
    Rule
        { rlhs :: [LAgent]
        , rrhs :: [RAgent e]
        , mults :: [e]
        , rexpr :: SEr e
        , cexpr :: SEr e
        }
    deriving (Show)

data SRule =
    SRule
        { lexps :: [Exp]
        , rexps :: [Exp]
        , multExps :: [Exp]
        , srate :: Exp
        , cond :: Exp
        }
    deriving (Show)
