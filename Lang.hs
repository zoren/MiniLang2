module Lang where

data Pattern c v
  = PWildcard
  | PAlias (Pattern c v) v
  | PConstant c
  | PApply (Pattern c v) (Pattern c v)
data Case p c v = Case (Pattern c v) (Expression p c v)
data Cases p c v
  = CSingle (Case p c v)
  | CNext (Case p c v) (Cases p c v)
data Expression p c v
  = EConstant c
  | EPrim p
  | EVariable v
  | ELambda (Cases p c v)
  | EApply (Expression p c v) (Expression p c v)

cases :: (Case p c v -> a) -> (Case p c v -> Cases p c v -> a) -> Cases p c v -> a
cases single next css = case css of
  CSingle cs -> single cs
  CNext cs css' -> next cs css'
