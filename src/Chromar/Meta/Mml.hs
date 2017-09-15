module Mml where

import Text.XML.Light
import Chromar.Meta.Types

import qualified Language.Haskell.TH.Syntax as H

---- type for a subset of mathml
type MNm = String

data MType
    = MReal
    | MBool

data UnOp
    = Min
    | Abs
    deriving (Show)

data BinOp
    = Quot
    | Div
    | Minus
    | Pow
    deriving (Show)

data NOp
    = Plus
    | Times
    deriving (Show)

data MExp
    = Con Double
    | Symb MNm
    | UExp UnOp
           MExp
    | BExp BinOp
           MExp
           MExp
    | NExp NOp
           [MExp]
    deriving (Show)


class XMLable a where
  toXML :: a -> Element

instance XMLable MExp where
  toXML e = undefined

instance XMLable MType where
  toXML t = undefined

instance (XMLable e, XMLable t) => XMLable (Chromar e t) where
  toXML m = undefined


toMmlExp :: H.Exp -> MExp
toMmlExp = undefined

toMmlType :: H.Type -> MType
toMmlType = undefined

{-

bimap toMmlExp toMMlType
to transform a Chromar H.Exp H.Type to Chromar MExp MType
and then can turn into xml using toXML
-}












