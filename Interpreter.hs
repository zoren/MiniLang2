module Interpreter (interpretClosed) where

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map

import Lang

type Environment c v = Map.Map v (Value c v)
type PrimFunction c v = Value c v -> Value c v
data Value c v
  = VConstant c
  | VPrim (PrimFunction c v)
  | VApply (Value c v) (Value c v)
  | VClosure [Case (PrimFunction c v) c v] (Environment c v)

emptyEnv :: Environment c v
emptyEnv = Map.empty

lookupEnv :: (Ord v) => v -> Environment c v -> Value c v
lookupEnv var = fromMaybe (error "variable not bound in environment") . Map.lookup var

insertEnv :: (Ord v) => v -> Value c v -> Environment c v -> Environment c v
insertEnv = Map.insert

mergeEnvs :: (Ord v) => Environment c v -> Environment c v -> Environment c v
mergeEnvs = Map.union

interpretClosed :: (Ord v, Eq c) => Program (PrimFunction c v) c v -> Environment c v
interpretClosed decls = interpretDecls decls
  where
    match pat value = case pat of
      PWildcard -> return emptyEnv
      PAlias aliasedPattern alias -> insertEnv alias value <$> match aliasedPattern value
      PConstant pc | VConstant vc <- value, pc == vc -> return emptyEnv
      PApply p1 p2 | VApply v1 v2 <- value -> liftM2 mergeEnvs (match p1 v1) (match p2 v2)
      _ -> fail "pattern did not match"

    apply v1 v2 = case v1 of
      VConstant {} -> VApply v1 v2
      VPrim primFunc -> primFunc v2
      VApply {} -> VApply v1 v2
      VClosure ccases cenvironment -> go ccases
        where
          go [] = error "pattern match not exhaustive"
          go (Case pat exp : ccases') =
            maybe (go ccases') (interpret exp . flip mergeEnvs cenvironment) $ match pat v2

    interpret expression enviroment = case expression of
      EConstant constant -> VConstant constant
      EPrim prim -> VPrim prim
      EVariable var -> lookupEnv var enviroment
      ELambda cases -> VClosure cases enviroment
      EApply e1 e2 -> apply (interpret e1 enviroment) (interpret e2 enviroment)

    interpretDecl env (ValueDeclaration pat exp) =
      mergeEnvs (fromMaybe (error "value declaration") $ match pat $ interpret exp env) env

    interpretDecls = foldl interpretDecl emptyEnv
