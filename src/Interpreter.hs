{-# LANGUAGE OverloadedStrings #-}
module Interpreter(
  interpretClosedExp
                  ) where

import           Constant
import           Control.Monad (liftM2)
import           Control.Monad.Fix (mfix)
import           Data.Char (chr, ord)
import           Data.IORef
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import           Lang
import Prelude hiding (readFile)

type Environment c v = Map.Map v (Value c v)
data Value c v
  = VConstant c
  | VApply (Value c v) (Value c v)
  | VPrim T.Text
  | VClosure [Case c v] (Environment c v)
  | VRef (IORef (Value c v))

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
               VApply {} -> "(" ++ show v2 ++ ")"
               _ -> show v2
      VPrim {} -> "<prim>"
      VClosure {} -> "<closure>"
      VRef {} -> "<ref>"

primMap :: (Ord v, Show v) => T.Text -> Value Constant v -> IO (Value Constant v)
primMap name arg = case (name, arg) of
  ("fix", _) -> mfix $ apply arg
  ("concat", VConstant (CAtom "T") `VApply` VConstant (CString s1) `VApply` VConstant (CString s2)) ->
    rvc $ CString $ T.append s1 s2
  ("strLen", VConstant(CString s)) -> rvc $ CInt $ T.length s
  ("index", VConstant (CAtom "T") `VApply` VConstant (CString s) `VApply` VConstant (CInt i)) -> rvc $ CChar $ T.index s i
  ("subString", VConstant (CAtom "T3") `VApply` VConstant (CString s) `VApply` VConstant (CInt start) `VApply` VConstant (CInt len)) ->
    rvc $ CString $ T.take len $ T.drop start s
  ("chr", VConstant(CInt c)) -> rvc $ CChar $ chr c
  ("ord", VConstant(CChar c)) -> rvc $ CInt $ ord c
  ("add", VConstant (CAtom "T") `VApply` VConstant (CInt i1) `VApply` VConstant (CInt i2)) -> rvc $ CInt $ i1 + i2
  ("sub", VConstant (CAtom "T") `VApply` VConstant (CInt i1) `VApply` VConstant (CInt i2)) -> rvc $ CInt $ i1 - i2
  ("mult", VConstant (CAtom "T") `VApply` VConstant (CInt i1) `VApply` VConstant (CInt i2)) -> rvc $ CInt $ i1 * i2
  ("intEq", VConstant (CAtom "T") `VApply` VConstant (CInt i1) `VApply` VConstant (CInt i2)) -> retBool $ i1 == i2
  ("intSle", VConstant (CAtom "T") `VApply` VConstant (CInt i1) `VApply` VConstant (CInt i2)) -> retBool $ i1 <= i2
  ("intSlt", VConstant (CAtom "T") `VApply` VConstant (CInt i1) `VApply` VConstant (CInt i2)) -> retBool $ i1 < i2
  ("charEq", VConstant (CAtom "T") `VApply` VConstant (CChar c1) `VApply` VConstant (CChar c2)) -> retBool $ c1 == c2
  -- unsafe stuff
  ("readFile", VConstant (CString filePath)) -> VConstant . CString <$> readFile (T.unpack filePath)
  ("newRef", _) -> VRef <$> newIORef arg
  ("readRef", VRef r) -> readIORef r
  ("writeRef", VConstant (CAtom "T") `VApply` VRef r `VApply` newValue) -> do
    writeIORef r newValue
    return newValue
  _ -> error $ "prim not known: " ++ show name ++ " " ++ show arg
  where
    rvc = return . VConstant
    retBool b = rvc $ CInt $ if b then 1 else 0

emptyEnv :: Environment c v
emptyEnv = Map.empty

lookupEnv :: (Ord v, Show v) => v -> Environment c v -> Value c v
lookupEnv var = fromMaybe (error $ "variable not bound in environment:" ++ show var) . Map.lookup var

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

apply :: (Ord v, Show v) => Value Constant v -> Value Constant v -> IO (Value Constant v)
apply v1 v2 = case v1 of
  VConstant {} -> return $ VApply v1 v2
  VPrim primFunc -> primMap primFunc v2
  VApply {} -> return $ VApply v1 v2
  VClosure ccases cenvironment -> go ccases
    where
      go [] = error $ "pattern match not exhaustive: " ++ show v2
      go (Case pat body : ccases') =
        maybe (go ccases') (interpret body . flip mergeEnvs cenvironment) $ match pat v2
  VRef {} -> error "cannot apply ref cell"

interpret :: (Ord v, Show v) => Expression Constant v -> Environment Constant v -> IO (Value Constant v)
interpret expression enviroment = case expression of
  EConstant constant -> return $ VConstant constant
  EPrim prim -> return $ VPrim prim
  EVariable var -> return $ lookupEnv var enviroment
  ELambda cases -> return $ VClosure cases enviroment
  EApply e1 e2 -> do
    v1 <- interpret e1 enviroment
    v2 <- interpret e2 enviroment
    apply v1 v2

interpretClosedExp :: (Ord v, Show v) => Expression Constant v -> IO (Value Constant v)
interpretClosedExp e = interpret e emptyEnv
