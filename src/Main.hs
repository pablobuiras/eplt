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
import System.Console.Haskeline hiding (handle)
import Control.Monad.Trans
import Control.Exception

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStr "Using these axioms: \n"
	  showLawPriority
          repl

mySettings = setComplete noCompletion defaultSettings

repl :: IO ()
repl= runInputT mySettings $ forever $
         do m <- getInputLine "> "
            case m of
              Nothing -> return ()
              Just l -> lift $ handle (\UserInterrupt -> putStrLn "Proof interrupted." >> repl) $ 
                  do when (null l) repl
                     case parse formula "<interactive>" l of
                       Left err -> print err
                       Right f -> do putStr "Checking..."
                                     case tauto f of
                                       Counter v ->
                                           do putStr "Formula is not a tautology. Counterexample: "
                                              mapM_ (putStr . show) (M.toList v)
                                              putStr "\n"
                                       Tauto -> do putStrLn "Formula is a tautology. Proving..."
                                                   case prove f of
                                                     (p,st) -> print p >> print st

--either print print . parse formula "<interactive>" l  -- use readline later
