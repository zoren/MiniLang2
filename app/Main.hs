module Main where

import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import           HaskellStyleParser
import           Interpreter
import qualified Lang as L
import           Surface
import           PrimEnv
import           SurfaceToCore
import           Text.Megaparsec

main :: IO ()
main = do
    lines <- getContents
    let prog = fromMaybe (error "could not parse") $ parseMaybe pprog $ T.pack lines
    print $ interpretClosedExp (L.mapPrim (\(PrimIndentifier name) -> primMap name) $ convert prog)
