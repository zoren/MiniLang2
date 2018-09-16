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
lspace sp = L.space sp lineComment empty
  where
    lineComment = char '#' *> void (takeWhileP (Just "character") (/= '\n'))

scn :: Parser ()
scn = lspace space1

sc :: Parser ()
sc = lspace $ void $ takeWhile1P Nothing $ \x -> x == ' ' || x == '\t'

indented p = L.indentGuard scn GT pos1 *> p

nonIndented = L.nonIndented scn

trailingChars :: Parser Text
trailingChars = takeWhileP Nothing $ \c -> isAlphaNum c || c == '_'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lowerId :: Parser Identifier
lowerId = lexeme (T.cons <$> lowerChar <*> trailingChars)

upperId :: Parser Identifier
upperId = lexeme (T.cons <$> upperChar <*> trailingChars)

primId :: Parser Identifier
primId = lexeme (char '$' *> trailingChars)

sym :: Char -> Parser ()
sym = void . lexeme . char

parens :: Parser a -> Parser a
parens = between (sym '(') (sym ')')

pcomb :: (a -> a -> a) -> Parser a -> Parser a
pcomb f pelem = foldl1 f <$> some pelem

pconstant :: Parser Constant
pconstant =
  choice
  [ CInt <$> lexeme L.decimal
  , CAtom <$> upperId
  , CString . T.pack <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))
  ]

ppattern :: Parser Pattern
ppattern =
  pcomb PApply $ choice
  [ PWildcard <$ sym '_'
  , PConstant <$> pconstant
  , PVariable <$> lowerId
  , PParenthesis <$> parens ppattern
  ]

pexpAtom :: Parser Expression
pexpAtom = choice
  [ EConstant <$> pconstant
  , EVariable <$> lowerId
  , EPrim <$> primId
  , ELambda <$ sym '\\' <*> sepBy1 (Case <$> ppattern <* sym '.' <*> pexp) (try $ indented $ sym '|')
  , EParenthesis <$> parens pexp
  ]

pexp :: Parser Expression
pexp = pcomb EApply pexpAtom

pdeclExp = pcomb EApply (try $ indented pexpAtom)

pdecl :: Parser Declaration
pdecl = ValueDeclaration <$> ppattern <* sym '=' <*> pdeclExp

pprog :: Parser Program
pprog = many $ nonIndented pdecl <* scn
