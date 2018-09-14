{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import           Control.Monad (liftM2)
import           Data.Function (fix)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Lang
import Data.Text (Text)

type Environment c v = Map.Map v (Value c v)
type PrimFunction c v = Value c v -> Value c v
data Value c v
  = VConstant c
  | VPrim (PrimFunction c v)
  | VApply (Value c v) (Value c v)
  | VClosure [Case (PrimFunction c v) c v] (Environment c v)

instance (Eq c, Eq v) => Eq (Value c v) where
    x == y = case (x, y) of
      (VConstant c1, VConstant c2) -> c1 == c2
--      (VPrim p1, VPrim p2) -> p1 == p2
      (VApply v11 v12, VApply v21 v22) -> v11 == v21 && v12 == v22
      _ -> False

instance (Show c, Show v) => Show (Value c v) where
    show x = case x of
      VConstant c -> show c
      VPrim {} -> "<prim>"
      VApply v1 v2 -> show v1 ++ " " ++ show v2
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

primMap :: Eq c => Text -> Value c Text -> Value c Text
primMap name = case name of
  "fix" -> fix . apply
  _ -> error "prim not known"

