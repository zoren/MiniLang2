{-# LANGUAGE OverloadedStrings #-}
import           HaskellStyleParser
import           Text.Megaparsec
import           Test.HUnit
import           Surface
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Lang as L
import           SurfaceToCore
import           Interpreter
import           Constant

unsafeParse parser t = fromMaybe (error $ "did not parse: " ++ T.unpack t) $ parseMaybe parser t

testParser :: (Eq a, Show a) => Parser a -> Text -> a -> Test
testParser parser t expected =
  TestLabel (T.unpack t) $ TestCase (assertEqual (T.unpack t) (unsafeParse parser t) expected)

pc = PConstant . CAtom
pv = PVariable
ec = EConstant . CAtom
ev = EVariable
eapp = EApply
papp = PApply

patternTests =
  let
    t = testParser ppattern
  in
  TestList
  [ t "_" PWildcard
  , t "Nil" $ pc "Nil"
  , t "(Nil)" $ PParenthesis $ pc "Nil"
  , t "( Nil )" $ PParenthesis $ pc "Nil"
  , t "x" $ pv "x"
  , t "Some x" $ pc "Some" `PApply` pv "x"
  ]

expressionTests =
  let
    t = testParser pexp
  in
  TestList
  [ t "Nil" $ ec "Nil"
  , t "42" $ EConstant $ CInt 42
  , t "\"abc\"" $ EConstant $ CString "abc"
  , t "$fix" $ EPrim "fix"
  , t "x" $ ev "x"
  , t "\\x.x" $ ELambda [pv "x" `Case` ev "x"]
  , t "\\x.x|y.y" $ ELambda [pv "x" `Case` ev "x"
                            ,pv "y" `Case` ev "y"]
  , t "\\x.S x" $ ELambda [pv "x" `Case` (ec "S" `EApply` ev "x")]
  , t "f x" $ EApply (ev "f") $ ev "x"
  , t "f x y" $ ev "f" `EApply` ev "x" `EApply` ev "y"
  , t "( Nil )" $ EParenthesis $ ec "Nil"
  ]

programTests =
  let
    t = testParser pprog
    vd = ValueDeclaration
  in
    TestList
    [ t "" []
    , t "x = Nil" [pv "x" `vd` ec "Nil"]
    , t "x = A\ny = B" [pv "x" `vd` ec "A"
                       ,pv "y" `vd` ec "B"]
    , t "x = A" [pv "x" `vd` ec "A"]
    , t "x = A B" [pv "x" `vd` (ec "A" `EApply` ec "B") ]
    , t "x = A   y" [pv "x" `vd` (ec "A" `EApply` ev "y") ]
    , t "f = \\x.x" [pv "f" `vd` ELambda [Case (pv "x") (ev "x")]]
    , t "f = \\Nil.None    |Cons x _.Some x" [pv "f" `vd` ELambda [Case (pc "Nil") (ec "None")
                                                                  ,Case (pc "Cons" `papp` pv "x" `papp` PWildcard) (ec "Some" `eapp` ev "x")]]
    ]

evalExpTests =
  let
    p = unsafeParse pexp
    i t = interpretClosedExp $ convertExpression $ p t
    e e1 e2 = test $ do
      v1 <- i e1
      v2 <- i e2
      assertEqual "" v2 v1
  in
    TestList
    [ e "(\\x.x)A" "A"
    , e "(\\_.A)C" "A"
    , e "(\\A.B)A" "B"
    , e "(\\A.B|A.C)A" "B"
    , e "(\\X.B|A.C)A" "C"
    , e "(\\A.B|C.D)C" "D"
    , e "(\\Some x.x)(Some A)" "A"
    , e "(\\Cons x xs.x)(Cons A Nil)" "A"
    , e "(\\Cons x xs.xs)(Cons A Nil)" "Nil"
    , e "(\\Cons x _.x)(Cons A Nil)" "A"
    , e "(\\Cons x _.x)(Cons 42 Nil)" "42"
    , e "(\\Cons x _.x)(Cons \"a\" Nil)" "\"a\""
    , e "(\\Cons x _.x)(Cons 'a' Nil)" "'a'"
    , e "(\\Nil.None|Cons x _.Some x) Nil" "None"
    , e "(\\Nil.None|Cons x _.Some x)(Cons A Nil)" "Some A"
    , e "($fix \\r.\\Nil.Z|Cons _ xs.S(r xs))(Cons B (Cons A Nil))" "S(S Z)"
    ]

tests = TestList
  [ patternTests
  , expressionTests
  , programTests
  , evalExpTests
  ]

main :: IO Counts
main = runTestTT tests
