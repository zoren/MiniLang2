module Surface where

import Data.Text

type Identifier = Text
newtype LowerIdentifier = LowerIdentifier Identifier
  deriving (Eq, Ord)

instance Show LowerIdentifier where
  show (LowerIdentifier name) = unpack name

newtype UpperIdentifier = UpperIdentifier Identifier
  deriving (Eq, Ord)

instance Show UpperIdentifier where
  show (UpperIdentifier name) = unpack name

data Pattern
  = PWildcard -- _
  | PConstant UpperIdentifier -- Cons
  | PApply Pattern Pattern -- Cons _ _
  | PParenthesis Pattern -- (Cons _ _)
  | PVariable LowerIdentifier -- x
  deriving (Eq, Show)

newtype PrimIndentifier = PrimIndentifier Identifier
  deriving (Eq, Show)
data Case = Case Pattern Expression
  deriving (Eq, Show)
data Expression
  = EConstant UpperIdentifier -- Nil, Cons
  | EPrim PrimIndentifier -- $eq
  | EVariable LowerIdentifier -- x
  | ELambda [Case]  -- | Nil . 0 | Cons _ _ . 1
  | EApply Expression Expression -- f 5
  | EParenthesis Expression -- f (g x)
  deriving (Eq, Show)
data Declaration
  = ValueDeclaration Pattern Expression -- x = 5
  deriving (Eq, Show)
type Program = [Declaration]
