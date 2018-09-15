module HaskellStyleParser where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Control.Monad (void)
import           Data.Char (
  isAlphaNum,
  )
import Data.Text (
  Text,
  )
import qualified Data.Text as T
import           Data.Void
import           Surface

type Parser a = Parsec Void Text a

lspace :: Parser () -> Parser ()
lspace sp = L.space sp empty empty

scn :: Parser ()
scn = lspace space1

sc :: Parser ()
sc = lspace $ void $ takeWhile1P Nothing $ \x -> x == ' ' || x == '\t'

trailingChars :: Parser Text
trailingChars = takeWhileP Nothing $ \c -> isAlphaNum c || c == '_'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lowerId :: Parser LowerIdentifier
lowerId = LowerIdentifier <$> lexeme (T.cons <$> lowerChar <*> trailingChars)

upperId :: Parser UpperIdentifier
upperId = UpperIdentifier <$> lexeme (T.cons <$> upperChar <*> trailingChars)

primId :: Parser PrimIndentifier
primId = PrimIndentifier <$> lexeme (T.cons <$> char '$' *> trailingChars)

sym :: Char -> Parser ()
sym = void . lexeme . char

parens :: Parser a -> Parser a
parens = between (sym '(') (sym ')')

pcomb :: (a -> a -> a) -> Parser a -> Parser a
pcomb f pelem = foldl1 f <$> some pelem

ppattern :: Parser Pattern
ppattern =
  pcomb PApply $ choice
  [ PWildcard <$ sym '_'
  , PConstant <$> upperId
  , PVariable <$> lowerId
  , PParenthesis <$> parens ppattern
  ]

pexp :: Parser Expression
pexp =
  pcomb EApply $ choice
  [ EConstant <$> upperId
  , EVariable <$> lowerId
  , EPrim <$> primId
  , ELambda <$ sym '\\' <*> sepBy1 (Case <$> ppattern <* sym '.' <*> pexp) (sym '|')
  , EParenthesis <$> parens pexp
  ]

pdecl :: Parser Declaration
pdecl = ValueDeclaration <$> ppattern <* sym '=' <*> pexp

pprog :: Parser Program
pprog = many $ L.nonIndented scn pdecl
