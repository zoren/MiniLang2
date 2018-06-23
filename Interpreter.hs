module Interpreter(interpretClosed) where

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)

import Lang

type Environment = [(VariableName, Value)]
data Value
  = VConstant Constant
  | VPrim PrimName
  | VApply Value Value
  | VClosure Cases Environment

emptyEnv :: Environment
emptyEnv = []

lookupEnv :: VariableName -> Environment -> Value
lookupEnv var = fromMaybe (error "variable not bound in environment") . lookup var

insertEnv :: VariableName -> Value -> Environment -> Environment
insertEnv var val = (:) (var, val)

mergeEnvs :: Environment -> Environment -> Environment
mergeEnvs = (++)

match :: Pattern -> Value -> Maybe Environment
match pat value = case pat of
  PWildcard -> Just []
  PAlias aliasedPattern alias -> insertEnv alias value <$> match aliasedPattern value
  PConstant pc -> case value of
    VConstant vc | pc == vc -> Just []
    _ -> Nothing
  PApply p1 p2 -> case value of
    VApply v1 v2 -> liftM2 mergeEnvs (match p1 v1) (match p2 v2)
    _ -> Nothing

evalPrim :: PrimName -> Value -> Value
evalPrim primName arg = case primName of
  "atomEq" -> case arg of
    VApply (VApply (VConstant "Pair") (VConstant v1)) (VConstant v2) ->
      VConstant $ if v1 == v2 then "True" else "False"
    _ -> error "atomEq got unexpected value"
  _ -> error "unknown prim"

apply :: Value -> Value -> Value
apply v1 v2 = case v1 of
  VConstant {} -> VApply v1 v2
  VPrim primName -> evalPrim primName v2
  VApply {} -> VApply v1 v2
  VClosure ccases cenviroment -> go ccases
    where
      go cs = let
        (Case pat exp, cont) =
          cases
          (\cs -> (cs, error "pattern match not exhaustive"))
          (\cs css' -> (cs, go css')) cs
        in maybe cont (\ patEnv -> interpret exp $ mergeEnvs patEnv cenviroment) $ match pat v2

interpret :: Expression -> Environment -> Value
interpret expression enviroment = case expression of
  EConstant constant -> VConstant constant
  EPrim prim -> VPrim prim
  EVariable var -> lookupEnv var enviroment
  ELambda cases -> VClosure cases enviroment
  EApply e1 e2 -> apply (interpret e1 enviroment) (interpret e2 enviroment)

interpretClosed :: Expression -> Value
interpretClosed e = interpret e emptyEnv