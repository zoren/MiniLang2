{-# LANGUAGE OverloadedStrings #-}
module PrimEnv where

import           Data.Char (chr, ord)
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
    \arg ->
      case arg of
        (VConstant(CString s)) -> VConstant $ CInt $ T.length s
        _ -> error $ "strLen unexpected arg: " ++ show arg
  "index" ->
    \arg ->
      case arg of
        (VConstant (CAtom "T") `VApply` (VConstant (CString s)) `VApply` (VConstant (CInt i))) -> VConstant $ CChar $ T.index s i
        _ -> error $ "concat unexpected arg: " ++ show arg
  "chr" ->
    \(VConstant(CInt c)) -> VConstant $ CChar $ chr c
  "ord" ->
    \(VConstant(CChar c)) -> VConstant $ CInt $ ord c
  "add" ->
    \arg ->
      case arg of
        (VConstant (CAtom "T") `VApply` (VConstant (CInt i1)) `VApply` (VConstant (CInt i2))) -> VConstant $ CInt $ i1 + i2
        _ -> error $ "concat unexpected arg: " ++ show arg
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
