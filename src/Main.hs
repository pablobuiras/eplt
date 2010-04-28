module Main where

import Prover
import Formula
import Formula.Parser (formula)
import Formula.Pretty
import Text.ParserCombinators.Parsec
import Data.Either
import System.IO
import Control.Monad

pf lf = rights $ map (parse formula "<interactive>") lf
parseformula f = head $ pf [f]

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          repl

repl :: IO ()
repl = forever $
         do putStr "> "
	    l <- getLine
	    if not (null l) then do
	                           print $ parseformula l  
			           putStr $ "Computing... "++ show (length $ takeWhile (\x -> not (x==FTrue)) $ step $ parseformula l) ++ " step(s) \nTrue!\n"
	                    else return ()
	    --either print print . parse formula "<interactive>" l  -- use readline later
