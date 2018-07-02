module HaskellStyleParser where

import Data.Text (Text)

import qualified Lang as L

type Location = Int

data Identifier = Identifier Location Text
newtype LowerIdentifier = LowerIdentifier Identifier
newtype UpperIdentifier = UpperIdentifier Identifier
data Pattern
  = PWildcard Location -- _
  | PAlias LowerIdentifier Location Pattern -- a @ _
  | PConstant UpperIdentifier -- Cons
  | PApply Pattern Pattern -- Cons _ _
  | PParenthesis Location Pattern Location -- (Cons _ _)

newtype PrimIndentifier = PrimIndentifier Identifier
data Case = Case Location Pattern Location Expression
data Expression
  = EConstant UpperIdentifier -- Nil, Cons
  | EPrim PrimIndentifier -- $eq
  | EVariable LowerIdentifier -- x
  | ELambda [Case]  -- \ Nil -> 0 | Cons _ _ -> 1
  | EApply Expression Expression -- f 5
  | EParenthesis Location Expression Location

convertPattern :: Pattern -> L.Pattern UpperIdentifier LowerIdentifier
convertPattern p = case p of
  PWildcard _ -> L.PWildcard
  PAlias lid _ p -> L.PAlias lid $ convertPattern p
  PConstant uid -> L.PConstant uid
  PApply p1 p2 -> L.PApply (convertPattern p1) (convertPattern p2)
  PParenthesis _ p _ -> convertPattern p

convertExpression :: Expression -> L.Expression PrimIndentifier UpperIdentifier LowerIdentifier
convertExpression e = case e of
  EConstant uid -> L.EConstant uid
  EPrim pid -> L.EPrim pid
  EVariable v -> L.EVariable v
  ELambda cases -> L.ELambda $ map (\(Case _ p _ e) -> L.Case (convertPattern p) (convertExpression e)) cases
  EApply e1 e2 -> L.EApply (convertExpression e1) (convertExpression e2)
  EParenthesis _ e _ -> convertExpression e
