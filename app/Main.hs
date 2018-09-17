module Main where

import           Data.Maybe
import qualified Data.Text as T
import           HaskellStyleParser
import           Interpreter
import qualified Lang as L
import           SurfaceToCore
import           Text.Megaparsec

main :: IO ()
main = do
  contents <- getContents
  let prog = fromMaybe (error "could not parse") $ parseMaybe pprog $ T.pack contents
  v <- interpretClosedExp $ convert prog
  print v
