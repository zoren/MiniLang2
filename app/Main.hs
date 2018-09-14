{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Megaparsec
import HaskellStyleParser
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Lang as L
import Surface
import SurfaceToCore
import Interpreter

main :: IO ()
main = do
    lines <- getContents
    let prog = fromMaybe (error "could not parse") $ parseMaybe pprog $ T.pack lines
    print $ interpretClosedExp (L.mapPrim (\(PrimIndentifier name) -> primMap name) $ convert prog)
