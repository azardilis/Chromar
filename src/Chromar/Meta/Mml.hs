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
    | And
    | Or  
    deriving (Show)

data MExp
    = ConB Bool
    | ConI Integer
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

defElemAttrs nm attrs cs =
    Element
    { elName =
        QName
        { qName = nm
        , qURI = Nothing
        , qPrefix = Nothing
        }
    , elAttribs = defAttrs attrs
    , elContent = cs
    , elLine = Nothing
    }

defAttrs attrs = map mkAttr attrs
  where
    mkAttr (nm, v) =
        Attr
        { attrKey =
            QName
            { qName = nm
            , qURI = Nothing
            , qPrefix = Nothing
            }
        , attrVal = v
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

mkOpApp op args = defElem "apply" ([Elem $ defElem op []] ++ args)

mkFnApp fnm args = defElem "apply" ([Elem $ defElem "csymbol" [textContent fnm]] ++ args)

class ToXML a where
  toXml :: a -> Element

---remember to use the mml namespace xmlns="http://www.w3.org/1998/Math/MathML"
instance ToXML MExp where
  toXml (ConB b) = defElemAttrs "cn" [("type", "bool")] [textContent $ show b]
  toXml (ConI n) = defElemAttrs "cn" [("type", "integer")] [textContent $ show n]
  toXml (ConD n) = defElemAttrs "cn" [("type", "real")] [textContent $ show n]
  toXml (Symb nm) = defElem "ci" [textContent nm]
  toXml (UExp Neg me) = mkOpApp "minus" [Elem $ toXml me]
  toXml (UExp Abs me) = mkOpApp "abs" [Elem $ toXml me]
  toXml (BExp Div me1 me2) = mkOpApp "divide" [Elem $ toXml me1, Elem $ toXml me2]
  toXml (BExp Minus me1 me2) = mkOpApp "minus" [Elem $ toXml me1, Elem $ toXml me2]
  toXml (BExp Pow me1 me2) = mkOpApp "power" [Elem $ toXml me1, Elem $ toXml me2]
  toXml (NExp Plus mes) = mkOpApp "plus" (map (Elem . toXml) mes)
  toXml (NExp Times mes) = mkOpApp "times" (map (Elem . toXml) mes)
  toXml (NExp And mes) = mkOpApp "and" (map (Elem . toXml) mes)
  toXml (NExp Or mes) = mkOpApp "or" (map (Elem . toXml) mes)
  toXml (MAppE fnm mes) = mkFnApp fnm (map (Elem . toXml) mes)

instance (Show a) => ToXML (AgentType a) where
    toXml (AgentType nm intf) =
        defElem "agentDecl" [Elem $ agentNmElem nm, Elem $ intfElem]
      where
        attrTypeElem t = defElem "attrType" [textContent $ show t]
        attrElem (attrNm, t) =
            defElem "attrDecl" [Elem $ attrNmElem attrNm, Elem $ attrTypeElem t]
        intfElem = defElem "intfDecl" $ map (Elem . attrElem) intf

instance ToXML LAgent where
  toXml (LAgent nm intf) =
      defElem "lAgent" [Elem $ agentNmElem nm, Elem $ intfElem]
    where
      attrBindElem v = defElem "varBind" [textContent v]
      attrElem (attrNm, v) =
          defElem "attrBind" [Elem $ attrNmElem attrNm, Elem $ attrBindElem v]
      intfElem = defElem "intfLAgent" $ map (Elem . attrElem) intf

instance (ToXML e) =>
         ToXML (RAgent e) where
    toXml (RAgent nm intf) =
        defElem "ragent" [Elem $ agentNmElem nm, Elem $ intfElem]
      where
        attrExprElem e = defElem "math" [Elem $ toXml e]
        rattrElem (attrNm, e) =
            defElem "attrExpr" [Elem $ attrNmElem attrNm, Elem $ attrExprElem e]
        intfElem = defElem "intfRAgent" $ map (Elem . rattrElem) intf

instance (ToXML e) =>
         ToXML (ARule e) where
    toXml Rule {lhs = lhs
               ,rhs = rhs
               ,rexpr = re
               ,cexpr = ce} =
        defElem
            "rule"
            [ Elem $ lhsElem 
            , Elem $ rhsElem 
            , Elem $ rexprElem 
            , Elem $ cexprElem
            ]
      where
        lhsElem = defElem "lhs" (map (Elem . toXml) lhs)
        rhsElem = defElem "rhs" (map (Elem . toXml) rhs)
        rexprElem = defElem "math" [Elem $ toXml re]
        cexprElem = defElem "math" [Elem $ toXml ce]

instance (ToXML e, ToXML t) => ToXML (Chromar e t) where
  toXml m = undefined

viewVar :: Exp -> String
viewVar (VarE nm) = show nm
viewVar _ = ""

viewT :: Type -> String
viewT (ConT nm) = show nm
viewT _ = ""

viewNm :: Name -> String
viewNm nm = show nm

toMmlExp :: Exp -> MExp
toMmlExp (LitE (IntegerL n)) = ConI n
toMmlExp (ConE (viewNm -> "GHC.Types.True")) = ConB True
toMmlExp (ConE (viewNm -> "GHC.Types.False")) = ConB False
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
toMmlExp (InfixE (Just e1) (viewVar -> "GHC.Classes.||") (Just e2)) =
    case (toMmlExp e2) of
        NExp Or es -> NExp Or (toMmlExp e1: es)
        me -> NExp Or [toMmlExp e1, me]
toMmlExp (InfixE (Just e1) (viewVar -> "GHC.Classes.&&") (Just e2)) =
    case (toMmlExp e2) of
        NExp And es -> NExp And (toMmlExp e1 : es)
        me -> NExp And [me, toMmlExp e2]
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

testRAgent :: IO ()
testRAgent = mapM_ print $ lines (ppElement (toXml ragent))
  where
    ragent = RAgent "A" [("x", NExp Plus [Symb "x", ConI 1])]

testMml :: MExp -> IO ()
testMml mml = mapM_ print $ lines (ppElement (toXml mml))

{-

bimap toMmlExp toMMlType
to transform a Chromar H.Exp H.Type to Chromar MExp MType
and then can turn into xml using toXML
-}
