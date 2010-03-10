module Main where

import Formula
import Formula.Parser (formula)
import Text.ParserCombinators.Parsec
import Data.Either
import System.IO
import Control.Monad

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          repl

repl :: IO ()
repl = forever $
         do putStr "> "
            either print print . parse formula "<interactive>" =<< getLine -- use readline later