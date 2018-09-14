{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import           Control.Monad (liftM2)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Lang

type Environment c v = Map.Map v (Value c v)
type PrimFunction c v = Value c v -> Value c v
data Value c v
  = VConstant c
  | VApply (Value c v) (Value c v)
  | VPrim (PrimFunction c v)
  | VClosure [Case (PrimFunction c v) c v] (Environment c v)

instance (Eq c, Eq v) => Eq (Value c v) where
    x == y = case (x, y) of
      (VConstant c1, VConstant c2) -> c1 == c2
      (VApply v11 v12, VApply v21 v22) -> v11 == v21 && v12 == v22
      (VPrim _, VPrim _) -> error "cannot compare prims"
      (VClosure {}, VClosure {}) -> error "cannot compare closures"
      _ -> error "incompatible comparison"

instance (Show c, Show v) => Show (Value c v) where
    show x = case x of
      VConstant c -> show c
      VApply v1 v2 ->
        show v1 ++ " " ++
             case v2 of
               VApply {} -> " (" ++ show v2 ++ ")"
               _ -> show v2
      VPrim {} -> "<prim>"
      VClosure {} -> "<closure>"

emptyEnv :: Environment c v
emptyEnv = Map.empty

lookupEnv :: (Ord v) => v -> Environment c v -> Value c v
lookupEnv var = fromMaybe (error "variable not bound in environment") . Map.lookup var

insertEnv :: (Ord v) => v -> Value c v -> Environment c v -> Environment c v
insertEnv = Map.insert

mergeEnvs :: (Ord v) => Environment c v -> Environment c v -> Environment c v
mergeEnvs = Map.union

match :: (Monad m, Ord v, Eq c) => Pattern c v -> Value c v -> m (Environment c v)
match pat value = case pat of
  PWildcard -> return emptyEnv
  PConstant pc | VConstant vc <- value, pc == vc -> return emptyEnv
  PAlias alias aliasedPattern -> insertEnv alias value <$> match aliasedPattern value
  PApply p1 p2 | VApply v1 v2 <- value -> liftM2 mergeEnvs (match p1 v1) (match p2 v2)
  _ -> fail "pattern did not match"

apply :: (Ord v, Eq c) => Value c v -> Value c v -> Value c v
apply v1 v2 = case v1 of
  VConstant {} -> VApply v1 v2
  VPrim primFunc -> primFunc v2
  VApply {} -> VApply v1 v2
  VClosure ccases cenvironment -> go ccases
    where
      go [] = error "pattern match not exhaustive"
      go (Case pat body : ccases') =
        maybe (go ccases') (interpret body . flip mergeEnvs cenvironment) $ match pat v2

interpret :: (Ord v, Eq c) => Expression (PrimFunction c v) c v -> Environment c v -> Value c v
interpret expression enviroment = case expression of
  EConstant constant -> VConstant constant
  EPrim prim -> VPrim prim
  EVariable var -> lookupEnv var enviroment
  ELambda cases -> VClosure cases enviroment
  EApply e1 e2 -> interpret e1 enviroment `apply` interpret e2 enviroment

interpretClosedExp :: (Ord v, Eq c) => Expression (PrimFunction c v) c v -> Value c v
interpretClosedExp e = interpret e emptyEnv
