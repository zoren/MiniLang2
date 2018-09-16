module Lang where

import Data.Text (Text)

data Pattern c v
  = PWildcard
  | PConstant c
  | PAlias v (Pattern c v)
  | PApply (Pattern c v) (Pattern c v)
data Case c v = Case (Pattern c v) (Expression c v)
data Expression c v
  = EConstant c
  | EPrim Text
  | EVariable v
  | ELambda [Case c v]
  | EApply (Expression c v) (Expression c v)
data Declaration c v
  = ValueDeclaration (Pattern c v) (Expression c v)
type Program c v = [Declaration c v]
