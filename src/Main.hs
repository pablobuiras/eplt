module Main where

import Prover
import Formula.Parser (parser)
import Formula.Pretty
import System.IO
import Control.Monad
import ModelBuilder (modelCheck)
import System.Console.Haskeline hiding (handle)
import Control.Monad.Trans
import Control.Exception
import System.Exit
import Exceptions
import Laws
import Configure.Load

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          lb <- loadLawsFrom "default.laws"
          putStr "Using these axioms: \n"
	  showLawPriority lb
          repl lb

exit = lift $ (putStrLn "Bye." >> exitWith ExitSuccess)

mySettings = setComplete noCompletion defaultSettings

repl :: LawBank -> IO ()
repl lb = runInputT mySettings $ forever $
          do m <- getInputLine "> "
             case m of
              Nothing -> exit
              Just [] -> return ()
              Just l -> lift $ flip catches [Handler (\UserInterrupt -> putStrLn "Proof interrupted." >> repl lb),
                                             Handler (\(SomeEPLTException e) -> print e)] $ 
                  do f <- parser "<interactive>" l
                     putStr "Checking..."
                     modelCheck f
                     putStrLn "Formula is a tautology. Proving..."
                     (p,st) <- prover lb f 
                     print p >> print st

