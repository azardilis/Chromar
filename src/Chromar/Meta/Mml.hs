{-# LANGUAGE ViewPatterns #-}

module Mml where

import Text.XML.Light
import Chromar.Meta.Types

import Language.Haskell.TH.Syntax

---- type for a subset of mathml
type MNm = String

data MType
    = MInt  
    | MReal
    | MBool

data UnOp
    = Neg
    | Abs
    deriving (Show)

data BinOp
    = Div
    | Minus
    | Pow
    deriving (Show)

data NOp
    = Plus
    | Times
    deriving (Show)

data MExp
    = ConI Integer
    | ConD Double
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

viewVar :: Exp -> String
viewVar (VarE nm) = show nm
viewVar _ = ""

viewT :: Type -> String
viewT (ConT nm) = show nm
viewT _ = ""

toMmlExp :: Exp -> MExp
toMmlExp (LitE (IntegerL n)) = ConI n
toMmlExp (VarE nm) = Symb (show nm)
toMmlExp (AppE (viewVar -> "GHC.Num.abs") e) = UExp Abs (toMmlExp e)
toMmlExp (AppE (viewVar -> "GHC.Num.negate") e) = UExp Neg (toMmlExp e)
toMmlExp (InfixE (Just e1) (viewVar -> "GHC.Real.^") (Just e2)) =
    BExp Pow (toMmlExp e1) (toMmlExp e2)
toMmlExp (InfixE (Just e1) (viewVar -> "GHC.Float.**") (Just e2)) =
    BExp Pow (toMmlExp e1) (toMmlExp e2)
toMmlExp (InfixE (Just e1) (viewVar -> "GHC.Num.-") (Just e2)) =
    BExp Minus (toMmlExp e1) (toMmlExp e2)
toMmlExp (InfixE (Just e1) (viewVar -> "GHC.Num./") (Just e2)) =
    BExp Div (toMmlExp e1) (toMmlExp e2)
toMmlExp (InfixE (Just e1) (viewVar -> "GHC.Num.+") (Just e2)) =
    case (toMmlExp e1) of
        NExp Plus es -> NExp Plus (es ++ [toMmlExp e2])
        me -> NExp Plus [me, toMmlExp e2]
toMmlExp (InfixE (Just e1) (viewVar -> "GHC.Num.*") (Just e2)) =
    case (toMmlExp e1) of
        NExp Times es -> NExp Times (es ++ [toMmlExp e2])
        me -> NExp Times [me, toMmlExp e2]
toMmlExp _ = undefined

toMmlType :: Type -> MType
toMmlType (viewT -> "GHC.Types.Int") = MInt
toMmlType (viewT -> "GHC.Types.Double") = MReal
toMmlType (viewT -> "GHC.Types.Bool") = MBool


exElem =
    Element
    { elName =
        QName
        { qName = "value"
        , qURI = Nothing
        , qPrefix = Nothing
        }
    , elAttribs = []
    , elContent =
        [ Text
              (CData
               { cdVerbatim = CDataRaw
               , cdData = "10"
               , cdLine = Nothing
               }),
          Text
              (CData
               { cdVerbatim = CDataRaw
               , cdData = "10"
               , cdLine = Nothing
               })
        ]
    , elLine = Nothing
    }
                           

testXML = ppElement exElem

{-

bimap toMmlExp toMMlType
to transform a Chromar H.Exp H.Type to Chromar MExp MType
and then can turn into xml using toXML
-}
