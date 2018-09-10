{-# LANGUAGE OverloadedStrings #-}
import HaskellStyleParser
import Text.Megaparsec
import Test.HUnit
import Surface
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

testParser :: (Eq a, Show a) => Parser a -> Text -> a -> Test
testParser parser t expected =
  TestLabel (T.unpack t) $ TestCase (assertEqual (T.unpack t) (fromMaybe (error "did not parse") $ parseMaybe parser t) expected)

pc = PConstant . UpperIdentifier
pv = PVariable . LowerIdentifier
ec = EConstant . UpperIdentifier
ev = EVariable . LowerIdentifier

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
  , t "$fix" $ EPrim $ PrimIndentifier "fix"
  , t "x" $ ev "x"
  , t "|x.x" $ ELambda [pv "x" `Case` ev "x"]
  , t "f x" $ EApply (ev "f") $ ev "x"
  , t "f x y" $ ev "f" `EApply` ev "x" `EApply` ev "y"
  , t "( Nil )" $ EParenthesis $ ec "Nil"
  ]

tests = TestList
  [ patternTests
  , expressionTests
  ]

main :: IO Counts
main = runTestTT tests

