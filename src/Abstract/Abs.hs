

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

