module Lang where

type PrimName = String
type Constant = String -- atom
type VariableName = String
data Pattern
  = PWildcard
  | PAlias Pattern VariableName
  | PConstant Constant
  | PApply Pattern Pattern
data Case = Case Pattern Expression
data Cases
  = CSingle Case
  | CNext Case Cases
data Expression
  = EConstant Constant
  | EPrim PrimName
  | EVariable VariableName
  | ELambda Cases
  | EApply Expression Expression

cases :: (Case -> a) -> (Case -> Cases -> a) -> Cases -> a
cases single next css = case css of
  CSingle cs -> single cs
  CNext cs css' -> next cs css'
