module Main where

import Prover
import Formula
import Formula.Parser (formula)
import Formula.Pretty
import Text.ParserCombinators.Parsec
import Data.Either
import qualified Data.Map as M
import System.IO
import Control.Monad
import ModelBuilder

pf lf = rights $ map (parse formula "<interactive>") lf
parseformula f = head $ pf [f]

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          repl

repl :: IO ()
repl = forever $
         do putStr "> "
	    l <- getLine
            when (null l) repl
            case parse formula "<interactive>" l of
              Left err -> print err
              Right f -> do putStr "Checking..."
                            case tauto f of
                              Counter v ->
                               do putStr "Formula is not a tautology. Counterexample: "
                                  mapM_ (putStr . show) (M.toList v)
                                  putStr "\n"
                              Tauto -> do putStr "Formula is a tautology. Proving..."
			                  putStr $ show (length $ takeWhile (\x -> not (x==FTrue)) $ step f) ++ " step(s) \nTrue!\n"
	    --either print print . parse formula "<interactive>" l  -- use readline later
