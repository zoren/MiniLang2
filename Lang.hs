module Lang where

type PrimName = String
type Constant = String -- atom
data Pattern v
  = PWildcard
  | PAlias (Pattern v) v
  | PConstant Constant
  | PApply (Pattern v) (Pattern v)
data Case v = Case (Pattern v) (Expression v)
data Cases v
  = CSingle (Case v)
  | CNext (Case v) (Cases v)
data Expression v
  = EConstant Constant
  | EPrim PrimName
  | EVariable v
  | ELambda (Cases v)
  | EApply (Expression v) (Expression v)

cases :: (Case v -> a) -> (Case v -> Cases v -> a) -> Cases v -> a
cases single next css = case css of
  CSingle cs -> single cs
  CNext cs css' -> next cs css'
