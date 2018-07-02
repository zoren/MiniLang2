module Lang where

data Pattern c v
  = PWildcard
  | PAlias v (Pattern c v)
  | PConstant c
  | PApply (Pattern c v) (Pattern c v)
data Case p c v = Case (Pattern c v) (Expression p c v)
data Expression p c v
  = EConstant c
  | EPrim p
  | EVariable v
  | ELambda [Case p c v]
  | EApply (Expression p c v) (Expression p c v)
data Declaration p c v
  = ValueDeclaration (Pattern c v) (Expression p c v)
type Program p c v = [Declaration p c v]
