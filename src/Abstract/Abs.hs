

type Multiset a = [(a, Int)]
type TEnv t = [(Nm, t)]

type Nm = String
type AttrName = String
type Var = String

data AgentType t = AgentType Nm [(AttrName, t)]

data RAgent e = RAgent Nm [(AttrName, e)]
data LAgent = LAgent Nm [(AttrName, Var)]

data ARule e = Rule { lhs :: [LAgent]
                   , rhs :: [RAgent e]
                   , rexpr :: e
                   , cexpr :: e }

data Chromar e t = Chromar { agentDecls :: [AgentType t]
                           , iState :: Multiset (Ragent e)
                           , rules :: [Rule e] }


instance Bifunctor Chromar where
  bimap ef tf = undefined

{-
then for every change in the 'inside language' we do bimap ...
for every change in the representation we need (le'ts take xml as an example)

instance (ToXML e, ToXML t) => ToXML (Chromar e t) where
   toXML (Chromar{..}) = ...
-} 

