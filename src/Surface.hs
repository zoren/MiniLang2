module Surface where

import Data.Text

data Position = Position Int Int
data Located a = Located Position a Position

type Identifier = Located Text
newtype LowerIdentifier = LowerIdentifier Identifier
newtype UpperIdentifier = UpperIdentifier Identifier

type Symbol = Located Char
data Pattern
  = PWildcard Symbol -- _
  | PConstant UpperIdentifier -- Cons
  | PAlias LowerIdentifier Symbol Pattern -- a @ _
  | PApply Pattern Pattern -- Cons _ _
  | PParenthesis Symbol Pattern Symbol -- (Cons _ _)

newtype PrimIndentifier = PrimIndentifier Identifier
data Case = Case Symbol Pattern Symbol Expression
data Expression
  = EConstant UpperIdentifier -- Nil, Cons
  | EPrim PrimIndentifier -- $eq
  | EVariable LowerIdentifier -- x
  | ELambda [Case]  -- \ Nil . 0 | Cons _ _ . 1
  | EApply Expression Expression -- f 5
  | EParenthesis Symbol Expression Symbol -- f (g x)
