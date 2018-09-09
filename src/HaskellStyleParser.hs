module HaskellStyleParser where

import Data.Text (
  Text,
  )


import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr


import           Data.Char (
  isAlphaNum,
  )
import qualified Data.Text as T
import           Data.Void
import Surface

type Parser a = Parsec Void Text a

position :: Parser Position
position = do
  pos <- getPosition
  return $ Position (unPos . sourceLine $ pos) (unPos . sourceColumn $ pos)

ppos :: Parser a -> Parser (Located a)
ppos p = Located <$> position <*> p <*> position

trailingChars :: Parser Text
trailingChars = takeWhileP Nothing $ \c -> isAlphaNum c || c == '_'

lowerId :: Parser Text
lowerId = T.cons <$> lowerChar <*> trailingChars

upperId :: Parser Text
upperId = T.cons <$> upperChar <*> trailingChars

primId :: Parser Text
primId = T.cons <$> char '$' *> trailingChars
