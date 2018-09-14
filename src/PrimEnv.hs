{-# LANGUAGE OverloadedStrings #-}
module PrimEnv where

import           Data.Function (fix)
import qualified Data.Text as T
import           Interpreter (apply, Value)
import           Surface

primMap :: Eq c => T.Text -> Value c LowerIdentifier -> Value c LowerIdentifier
primMap name = case name of
  "fix" -> fix . apply
  _ -> error "prim not known"