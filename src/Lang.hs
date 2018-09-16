module Lang where

data Pattern c v
  = PWildcard
  | PConstant c
  | PAlias v (Pattern c v)
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

mapPrim :: (p -> p') -> Expression p c v -> Expression p' c v
mapPrim f = go
  where
    go e = case e of
      EConstant c -> EConstant c
      EPrim p -> EPrim $ f p
      EVariable v -> EVariable v
      ELambda cases -> ELambda $ map (\(Case p body) -> Case p $ go body) cases
      EApply e1 e2 -> EApply (go e1) (go e2)
