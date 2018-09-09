module SurfaceToCore where

import Surface
import qualified Lang as L

convertPattern :: Pattern -> L.Pattern UpperIdentifier LowerIdentifier
convertPattern p = case p of
  PWildcard _ -> L.PWildcard
  PConstant uid -> L.PConstant uid
  PAlias lid _ p' -> L.PAlias lid $ convertPattern p'
  PApply p1 p2 -> L.PApply (convertPattern p1) (convertPattern p2)
  PParenthesis _ p' _ -> convertPattern p'

convertExpression :: Expression -> L.Expression PrimIndentifier UpperIdentifier LowerIdentifier
convertExpression e = case e of
  EConstant uid -> L.EConstant uid
  EPrim pid -> L.EPrim pid
  EVariable v -> L.EVariable v
  ELambda cases ->
    L.ELambda $ map (\(Case _ p _ e') -> L.Case (convertPattern p) (convertExpression e')) cases
  EApply e1 e2 -> L.EApply (convertExpression e1) (convertExpression e2)
  EParenthesis _ e' _ -> convertExpression e'
