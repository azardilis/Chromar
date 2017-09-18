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
    | MBool deriving (Show)

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
    | UExp UnOp MExp
    | BExp BinOp MExp MExp
    | NExp NOp [MExp]
    | MAppE MNm [MExp]       
    deriving (Show)


defElem nm cs =
    Element
    { elName =
        QName
        { qName = nm
        , qURI = Nothing
        , qPrefix = Nothing
        }
    , elAttribs = []
    , elContent = cs
    , elLine = Nothing
    }

textContent s =
    Text
        (CData
         { cdVerbatim = CDataRaw
         , cdData = s
         , cdLine = Nothing
         })

agentNmElem nm = defElem "agentNm" [textContent nm]

attrNmElem attrNm = defElem "attrName" [textContent attrNm]

class XMLable a where
  toXml :: a -> Element

instance XMLable MExp where
  toXml e = undefined

instance (XMLable e, XMLable t) => XMLable (Chromar e t) where
  toXml m = undefined

instance (Show a) => XMLable (AgentType a) where
    toXml (AgentType nm intf) =
        defElem "agentDecl" [Elem $ agentNmElem nm, Elem $ intfElem]
      where
        attrTypeElem t = defElem "attrType" [textContent $ show t]
        attrElem (attrNm, t) =
            defElem "attrDecl" [Elem $ attrNmElem attrNm, Elem $ attrTypeElem t]
        intfElem = defElem "intfDecl" $ map (Elem . attrElem) intf

instance XMLable LAgent where
  toXml (LAgent nm intf) =
      defElem "lAgent" [Elem $ agentNmElem nm, Elem $ intfElem]
    where
      attrBindElem v = defElem "varBind" [textContent v]
      attrElem (attrNm, v) =
          defElem "attrBind" [Elem $ attrNmElem attrNm, Elem $ attrBindElem v]
      intfElem = defElem "intfLAgent" $ map (Elem . attrElem) intf

instance (XMLable e) => XMLable (RAgent e) where
  toXml (RAgent nm intf) = undefined

instance (XMLable e) => XMLable (ARule e) where
  toXml Rule {lhs = lhs
             ,rhs = rhs
             ,rexpr = re
             ,cexpr = ce} = undefined

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
toMmlExp (AppE (viewVar -> (c:cs)) e) = MAppE (c:cs) [toMmlExp e]
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
toMmlExp (AppE e1 e2) =
    case (toMmlExp e1) of
        MAppE c es -> MAppE c (es ++ [toMmlExp e2])
toMmlExp _ = undefined

toMmlType :: Type -> MType
toMmlType (viewT -> "GHC.Types.Int") = MInt
toMmlType (viewT -> "GHC.Types.Double") = MReal
toMmlType (viewT -> "GHC.Types.Bool") = MBool


testAgentType :: IO ()
testAgentType = mapM_ print $ lines (ppElement (toXml agentt))
  where
    agentt = AgentType "A" [("x", MInt)]

testLAgent :: IO ()
testLAgent = mapM_ print $ lines (ppElement (toXml ragent))
  where
    ragent = LAgent "A" [("x", "x")]

{-

bimap toMmlExp toMMlType
to transform a Chromar H.Exp H.Type to Chromar MExp MType
and then can turn into xml using toXML
-}
