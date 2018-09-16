module Constant where

import Data.Text

data Constant
  = CAtom Text -- Nil, Cons
  | CInt Int -- 0, 1, 2, 42
  | CString Text -- "", "a"
  | CChar Char -- 'a', `\n`
  deriving (Eq)

instance Show Constant where
  show c = case c of
    CAtom a -> unpack a
    CInt i -> show i
    CString s -> show s
    CChar ch -> show ch
