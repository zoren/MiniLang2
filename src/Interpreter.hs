{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import           Constant
import           Control.Monad (liftM2)
import           Data.Char (chr, ord)
import           Data.Function (fix)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Lang

type Environment c v = Map.Map v (Value c v)
data Value c v
  = VConstant c
  | VApply (Value c v) (Value c v)
  | VPrim T.Text
  | VClosure [Case c v] (Environment c v)

type Identifier = T.Text

primMap :: (Ord v, Show v) => T.Text -> Value Constant v -> Value Constant v
primMap name = case name of
  "fix" -> fix . apply
  "concat" ->
    \arg ->
      case arg of
        (VConstant (CAtom "T") `VApply` (VConstant (CString s1)) `VApply` (VConstant (CString s2))) -> VConstant $ CString $ T.append s1 s2
        _ -> error $ "concat unexpected arg: " ++ show arg
  "strLen" ->
    \arg ->
      case arg of
        (VConstant(CString s)) -> VConstant $ CInt $ T.length s
        _ -> error $ "strLen unexpected arg: " ++ show arg
  "index" ->
    \arg ->
      case arg of
        (VConstant (CAtom "T") `VApply` (VConstant (CString s)) `VApply` (VConstant (CInt i))) -> VConstant $ CChar $ T.index s i
        _ -> error $ "concat unexpected arg: " ++ show arg
  "subString" ->
    \arg ->
      case arg of
        (VConstant (CAtom "T3") `VApply` (VConstant (CString s)) `VApply` (VConstant (CInt start)) `VApply` (VConstant (CInt len))) ->
          VConstant $ CString $ T.take len $ T.drop start s
        _ -> error $ "subString unexpected arg: " ++ show arg
  "chr" ->
    \(VConstant(CInt c)) -> VConstant $ CChar $ chr c
  "ord" ->
    \(VConstant(CChar c)) -> VConstant $ CInt $ ord c
  "add" ->
    \arg ->
      case arg of
        (VConstant (CAtom "T") `VApply` (VConstant (CInt i1)) `VApply` (VConstant (CInt i2))) -> VConstant $ CInt $ i1 + i2
        _ -> error $ "add unexpected arg: " ++ show arg
  "sub" ->
    \arg ->
      case arg of
        (VConstant (CAtom "T") `VApply` (VConstant (CInt i1)) `VApply` (VConstant (CInt i2))) -> VConstant $ CInt $ i1 - i2
        _ -> error $ "sub unexpected arg: " ++ show arg
  "mult" ->
    \arg ->
      case arg of
        (VConstant (CAtom "T") `VApply` (VConstant (CInt i1)) `VApply` (VConstant (CInt i2))) -> VConstant $ CInt $ i1 * i2
        _ -> error $ "mult unexpected arg: " ++ show arg
  "intEq" ->
    \arg ->
      case arg of
        (VConstant (CAtom "Q") `VApply` (VConstant (CInt i1)) `VApply` (VConstant (CInt i2)) `VApply` v1 `VApply` v2) ->
          if i1 == i2 then v1 else v2
        _ -> error $ "intEq unexpected arg: " ++ show arg
  "intLteq" ->
    \arg ->
      case arg of
        (VConstant (CAtom "Q") `VApply` (VConstant (CInt i1)) `VApply` (VConstant (CInt i2)) `VApply` v1 `VApply` v2) ->
          if i1 <= i2 then v1 else v2
        _ -> error $ "intEq unexpected arg: " ++ show arg
  _ -> error "prim not known"

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

apply :: (Ord v, Show v) => Value Constant v -> Value Constant v -> Value Constant v
apply v1 v2 = case v1 of
  VConstant {} -> VApply v1 v2
  VPrim primFunc -> primMap primFunc v2
  VApply {} -> VApply v1 v2
  VClosure ccases cenvironment -> go ccases
    where
      go [] = error "pattern match not exhaustive"
      go (Case pat body : ccases') =
        maybe (go ccases') (interpret body . flip mergeEnvs cenvironment) $ match pat v2

interpret :: (Ord v, Show v) => Expression Constant v -> Environment Constant v -> Value Constant v
interpret expression enviroment = case expression of
  EConstant constant -> VConstant constant
  EPrim prim -> VPrim prim
  EVariable var -> lookupEnv var enviroment
  ELambda cases -> VClosure cases enviroment
  EApply e1 e2 -> interpret e1 enviroment `apply` interpret e2 enviroment

interpretClosedExp :: (Ord v, Show v) => Expression Constant v -> Value Constant v
interpretClosedExp e = interpret e emptyEnv
