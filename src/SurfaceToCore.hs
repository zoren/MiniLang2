module SurfaceToCore where

import Surface
import qualified Lang as L

convertPattern :: Pattern -> L.Pattern Identifier Identifier
convertPattern p = case p of
  PWildcard -> L.PWildcard
  PConstant (UpperIdentifier uid) -> L.PConstant uid
  PApply p1 p2 -> L.PApply (convertPattern p1) (convertPattern p2)
  PParenthesis p' -> convertPattern p'
  PVariable (LowerIdentifier v) -> L.PAlias v L.PWildcard

convertExpression :: Expression -> L.Expression PrimIndentifier Identifier Identifier
convertExpression e = case e of
  EConstant (UpperIdentifier uid) -> L.EConstant uid
  EPrim pid -> L.EPrim pid
  EVariable (LowerIdentifier v) -> L.EVariable v
  ELambda cases -> L.ELambda $ map (\(Case p e') -> L.Case (convertPattern p) (convertExpression e')) cases
  EApply e1 e2 -> L.EApply (convertExpression e1) (convertExpression e2)
  EParenthesis e' -> convertExpression e'

convert :: Program -> L.Expression PrimIndentifier Identifier Identifier
convert [] = error "no decls"
convert [ValueDeclaration p e] = case p of
  PWildcard -> convertExpression e
  _ -> error "could not convert"
convert (ValueDeclaration p e:ds) = L.EApply (L.ELambda [L.Case (convertPattern p) (convert ds)]) $ convertExpression e
