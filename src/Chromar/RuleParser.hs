module Chromar.RuleParser where

import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Language.Haskell.TH
import Language.Haskell.Meta.Parse
import Control.Monad


data SRule = SRule { lexps :: [Exp],
                     rexps :: [Exp],
                     srate  :: Exp,
                     cond  :: Exp } deriving (Show)
                             

parseAgent :: Parser String
parseAgent = do
  spaces
  skipMany newline
  s <- many1 (noneOf ['}'])
  char '}'
  spaces
  skipMany newline
  return (s ++ "}")


parseRuleSide :: Parser [String]
parseRuleSide = do
  spaces
  skipMany newline
  spaces
  sepBy parseAgent (char ',')


arrowSpaces :: Parser ()
arrowSpaces = do
  spaces
  string "-->"
  spaces


parseRate :: Parser String
parseRate = do
  char '@'
  spaces
  skipMany newline
  spaces
  many1 (noneOf ['['])


parseCond :: Parser String
parseCond = do
  char '['
  spaces
  p <- many1 (noneOf "]")
  char ']'
  return p
  

createExps :: [String] -> [Exp]
createExps exps = case mapM parseExp exps of
  Left  s     -> error s
  Right pexps -> pexps


createPats :: [String] -> [Pat]
createPats pats = case mapM parsePat pats of
  Left s      -> error s
  Right ppats -> ppats


createExp :: String -> Exp
createExp exp = case parseExp exp of
  Left s    -> error s
  Right exp -> exp


parseRule :: Parser SRule
parseRule = do
  left <- parseRuleSide
  arrowSpaces
  right <- parseRuleSide
  spaces
  skipMany newline
  spaces
  r <- parseRate
  spaces
  c <- parseCond
  return SRule{ lexps = createExps left,
                rexps = createExps right,
                srate  = createExp r,
                cond  = createExp c }

--- for testing
readExpr :: String -> SRule
readExpr input = case parse parseRule "rules" input of
    Left err  -> error (show err)
    Right val -> val
