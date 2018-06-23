module Lang where

data Pattern c v
  = PWildcard
  | PAlias (Pattern c v) v
  | PConstant c
  | PApply (Pattern c v) (Pattern c v)
data Case p c v = Case (Pattern c v) (Expression p c v)
data Expression p c v
  = EConstant c
  | EPrim p
  | EVariable v
  | ELambda [Case p c v]
  | EApply (Expression p c v) (Expression p c v)
