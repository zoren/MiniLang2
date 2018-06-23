module Interpreter (interpretClosed) where

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)

import Lang

type Environment v = [(v, Value v)]
data Value v
  = VConstant Constant
  | VPrim PrimName
  | VApply (Value v) (Value v)
  | VClosure (Cases v) (Environment v)

emptyEnv :: Environment v
emptyEnv = []

lookupEnv :: (Eq v) => v -> Environment v -> Value v
lookupEnv var = fromMaybe (error "variable not bound in environment") . lookup var

insertEnv :: v -> Value v -> Environment v -> Environment v
insertEnv var val = (:) (var, val)

mergeEnvs :: Environment v -> Environment v -> Environment v
mergeEnvs = (++)

match :: Pattern v -> Value v -> Maybe (Environment v)
match pat value = case pat of
  PWildcard -> return emptyEnv
  PAlias aliasedPattern alias -> insertEnv alias value <$> match aliasedPattern value
  PConstant pc | VConstant vc <- value, pc == vc -> return emptyEnv
  PApply p1 p2 | VApply v1 v2 <- value -> liftM2 mergeEnvs (match p1 v1) (match p2 v2)
  _ -> fail "pattern did not match"

evalPrim :: PrimName -> Value v -> Value v
evalPrim primName arg = case primName of
  "atomEq" -> case arg of
    VApply (VApply (VConstant "Pair") (VConstant v1)) (VConstant v2) ->
      VConstant $ if v1 == v2 then "True" else "False"
    _ -> error "atomEq got unexpected value"
  _ -> error "unknown prim"

apply :: (Eq v) => Value v -> Value v -> Value v
apply v1 v2 = case v1 of
  VConstant {} -> VApply v1 v2
  VPrim primName -> evalPrim primName v2
  VApply {} -> VApply v1 v2
  VClosure ccases cenvironment -> go ccases
    where
      go cs = let
        (Case pat exp, cont) =
          cases
          (\cs -> (cs, error "pattern match not exhaustive"))
          (\cs css' -> (cs, go css')) cs
        in maybe cont (interpret exp . flip mergeEnvs cenvironment) $ match pat v2

interpret :: (Eq v) => Expression v -> Environment v -> Value v
interpret expression enviroment = case expression of
  EConstant constant -> VConstant constant
  EPrim prim -> VPrim prim
  EVariable var -> lookupEnv var enviroment
  ELambda cases -> VClosure cases enviroment
  EApply e1 e2 -> apply (interpret e1 enviroment) (interpret e2 enviroment)

interpretClosed :: (Eq v) => Expression v -> Value v
interpretClosed e = interpret e emptyEnv
