import Data.Maybe (fromMaybe)

type PrimName = String
type Constant = String -- atom
type VariableName = String
data Pattern
  = PWildcard
  | PConstant Constant
  | PAlias Pattern VariableName
  | PApply Pattern Pattern
data Case = Case Pattern Expression
data Cases
  = CSingle Case
  | CNext Case Cases
data Expression
  = EConstant Constant
  | EVariable VariableName
  | EPrim PrimName
  | ELambda Cases
  | EApply Expression Expression

cases :: (Case -> a) -> (Case -> Cases -> a) -> Cases -> a
cases single next css = case css of
  CSingle cs -> single cs
  CNext cs css' -> next cs css'

-- interpreter
  
type Environment = [(VariableName, Value)]
data Value
  = VConstant Constant
  | VApply Value Value
  | VPrim PrimName
  | VClosure Cases Environment

lookupEnv :: VariableName -> Environment -> Value
lookupEnv var = fromMaybe (error "variable not bound in environment") . lookup var

insertEnv :: VariableName -> Value -> Environment -> Environment
insertEnv var val = (:) (var, val)

match :: Pattern -> Environment -> Value -> Maybe Environment
match pat enviroment value =
  case pat of
    PWildcard -> Just enviroment
    PConstant pc ->
      case value of
        VConstant vc | pc == vc -> Just enviroment
        _ -> Nothing
    PAlias aliasedPattern alias -> do
      env' <- match aliasedPattern enviroment value
      return $ insertEnv alias value (env' ++ enviroment)
    PApply p1 p2 ->
      case value of
        VApply v1 v2 -> do
          env1 <- match p1 enviroment v1
          match p2 env1 v2
        _ -> Nothing

evalPrim :: PrimName -> Value -> Value
evalPrim primName arg = case primName of
  "atomEq" -> case arg of
    VApply (VApply (VConstant "Pair") (VConstant v1)) (VConstant v2) ->
      VConstant $ if v1 == v2 then "True" else "False"
    _ -> error "atomEq got unexpected value"
  _ -> error "unknown prim"

interpretCases varg cenviroment = go
  where
    go cs = let
      (Case pat exp, cont) =
        cases
        (\cs -> (cs, error "pattern match not exhaustive"))
        (\cs css' -> (cs, go css')) cs
      in maybe cont (interpret exp) $ match pat cenviroment varg

apply :: Value -> Value -> Value
apply v1 v2 = case v1 of
  VConstant {} -> VApply v1 v2
  VApply {} -> VApply v1 v2
  VPrim primName -> evalPrim primName v2
  VClosure ccases cenviroment -> interpretCases v2 cenviroment ccases
  
interpret :: Expression -> Environment -> Value
interpret expression enviroment =
  case expression of
    EConstant constant -> VConstant constant
    EVariable var -> lookupEnv var enviroment
    EPrim prim -> VPrim prim
    ELambda cases -> VClosure cases enviroment
    EApply e1 e2 -> apply (interpret e1 enviroment) (interpret e2 enviroment)
