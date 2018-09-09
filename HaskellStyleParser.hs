module HaskellStyleParser where

import qualified Lang as L

type Location = Int

data Identifier = Identifier Location String
newtype LowerIdentifier = LowerIdentifier Identifier
newtype UpperIdentifier = UpperIdentifier Identifier
data Pattern
  = PWildcard Location -- _
  | PConstant UpperIdentifier -- Cons
  | PAlias LowerIdentifier Location Pattern -- a @ _
  | PApply Pattern Pattern -- Cons _ _
  | PParenthesis Location Pattern Location -- (Cons _ _)

newtype PrimIndentifier = PrimIndentifier Identifier
data Case = Case Location Pattern Location Expression
data Expression
  = EConstant UpperIdentifier -- Nil, Cons
  | EPrim PrimIndentifier -- $eq
  | EVariable LowerIdentifier -- x
  | ELambda [Case]  -- \ Nil . 0 | Cons _ _ . 1
  | EApply Expression Expression -- f 5
  | EParenthesis Location Expression Location

convertPattern :: Pattern -> L.Pattern UpperIdentifier LowerIdentifier
convertPattern p = case p of
  PWildcard _ -> L.PWildcard
  PConstant uid -> L.PConstant uid
  PAlias lid _ p' -> L.PAlias lid $ convertPattern p'
  PApply p1 p2 -> L.PApply (convertPattern p1) (convertPattern p2)
  PParenthesis _ p' _ -> convertPattern p'

convertExpression :: Expression -> L.Expression PrimIndentifier UpperIdentifier LowerIdentifier
convertExpression e = case e of
  EConstant uid -> L.EConstant uid
  EPrim pid -> L.EPrim pid
  EVariable v -> L.EVariable v
  ELambda cases ->
    L.ELambda $ map (\(Case _ p _ e') -> L.Case (convertPattern p) (convertExpression e')) cases
  EApply e1 e2 -> L.EApply (convertExpression e1) (convertExpression e2)
  EParenthesis _ e' _ -> convertExpression e'
