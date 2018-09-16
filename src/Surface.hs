module Surface where

import Data.Text

type Identifier = Text

data Constant
  = CAtom Identifier -- Nil, Cons
  | CInt Int -- 0, 1, 2, 42
  | CString Text -- "", "a"
  | CChar Char -- 'a', `\n`
  deriving (Eq, Show)

data Pattern
  = PWildcard -- _
  | PConstant Constant -- Cons
  | PApply Pattern Pattern -- Cons _ _
  | PParenthesis Pattern -- (Cons _ _)
  | PVariable Identifier -- x
  deriving (Eq, Show)

newtype PrimIndentifier = PrimIndentifier Identifier
  deriving (Eq, Show)
data Case = Case Pattern Expression
  deriving (Eq, Show)
data Expression
  = EConstant Constant
  | EPrim Identifier -- $eq
  | EVariable Identifier -- x
  | ELambda [Case]  -- | Nil . 0 | Cons _ _ . 1
  | EApply Expression Expression -- f 5
  | EParenthesis Expression -- f (g x)
  deriving (Eq, Show)
data Declaration
  = ValueDeclaration Pattern Expression -- x = 5
  deriving (Eq, Show)
type Program = [Declaration]
