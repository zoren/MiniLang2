{-# LANGUAGE OverloadedStrings #-}
module PrimEnv where

import           Data.Function (fix)
import qualified Data.Text as T
import           Interpreter (apply, Value(..))
import           Surface

primMap :: T.Text -> Value Constant Identifier -> Value Constant Identifier
primMap name = case name of
  "fix" -> fix . apply
  "concat" ->
    \arg ->
      case arg of
        (VConstant (CAtom "T") `VApply` (VConstant (CString s1)) `VApply` (VConstant (CString s2))) -> VConstant $ CString $ T.append s1 s2
        _ -> error $ "concat unexpected arg: " ++ show arg
  "strLen" ->
    \(VConstant(CString s)) -> VConstant $ CInt $ T.length s
  "index" ->
    \arg ->
      case arg of
        (VConstant (CAtom "T") `VApply` (VConstant (CString s)) `VApply` (VConstant (CInt i))) -> VConstant $ CChar $ T.index s i
        _ -> error $ "concat unexpected arg: " ++ show arg
  _ -> error "prim not known"
