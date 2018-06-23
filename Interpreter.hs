module Interpreter (interpretClosed) where

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)

import Lang

type Environment p c v = [(v, Value p c v)]
data Value p c v
  = VConstant c
  | VPrim p
  | VApply (Value p c v) (Value p c v)
  | VClosure (Cases p c v) (Environment p c v)

emptyEnv :: Environment p c v
emptyEnv = []

lookupEnv :: (Eq v) => v -> Environment p c v -> Value p c v
lookupEnv var = fromMaybe (error "variable not bound in environment") . lookup var

insertEnv :: v -> Value p c v -> Environment p c v -> Environment p c v
insertEnv var val = (:) (var, val)

mergeEnvs :: Environment p c v -> Environment p c v -> Environment p c v
mergeEnvs = (++)

interpretClosed :: (Eq v, Eq c) => (p -> Value p c v -> Value p c v) -> Expression p c v -> Value p c v
interpretClosed evalPrim = flip interpret emptyEnv
  where
    match pat value = case pat of
      PWildcard -> return emptyEnv
      PAlias aliasedPattern alias -> insertEnv alias value <$> match aliasedPattern value
      PConstant pc | VConstant vc <- value, pc == vc -> return emptyEnv
      PApply p1 p2 | VApply v1 v2 <- value -> liftM2 mergeEnvs (match p1 v1) (match p2 v2)
      _ -> fail "pattern did not match"

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

    interpret expression enviroment = case expression of
      EConstant constant -> VConstant constant
      EPrim prim -> VPrim prim
      EVariable var -> lookupEnv var enviroment
      ELambda cases -> VClosure cases enviroment
      EApply e1 e2 -> apply (interpret e1 enviroment) (interpret e2 enviroment)
