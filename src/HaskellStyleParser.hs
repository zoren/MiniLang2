module HaskellStyleParser where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad (void)
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

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

trailingChars :: Parser Text
trailingChars = takeWhileP Nothing $ \c -> isAlphaNum c || c == '_'

lowerId :: Parser LowerIdentifier
lowerId = LowerIdentifier <$> lexeme (T.cons <$> lowerChar <*> trailingChars)

upperId :: Parser UpperIdentifier
upperId = UpperIdentifier <$> lexeme (T.cons <$> upperChar <*> trailingChars)

primId :: Parser PrimIndentifier
primId = PrimIndentifier <$> lexeme (T.cons <$> char '$' *> trailingChars)

sym :: Char -> Parser ()
sym c = void $ lexeme $ char c

ppattern :: Parser Pattern
ppattern = foldl1 PApply <$> some patom
  where
    patom =
      choice
      [ PWildcard <$ sym '_'
      , PConstant <$> upperId
      , PParenthesis <$ sym '(' <*> ppattern <* sym ')'
      , PVariable <$> lowerId
      ]

pexp :: Parser Expression
pexp = foldl1 EApply <$> some eatom
  where
  eatom =
    choice
    [ EConstant <$> upperId
    , EPrim <$> primId
    , EVariable <$> lowerId
    , ELambda <$ sym '\\' <*> sepBy1 lcase (sym '|')
    , EParenthesis <$ sym '(' <*> pexp <* sym ')'
    ]
  lcase = Case <$> ppattern <* sym '.' <*> pexp

pdecl :: Parser Declaration
pdecl = ValueDeclaration <$> ppattern <* sym '=' <*> pexp
