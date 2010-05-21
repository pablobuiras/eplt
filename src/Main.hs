module Main where

import Prover
import Formula.Pretty
import System.IO
import Control.Monad
import ModelBuilder (modelCheck)
import System.Console.Haskeline hiding (catch)
import Control.Monad.Trans
import Control.Exception
import System.Exit
import Exceptions
import Laws
import Configure.Load
import Commands
import Commands.Parser
import Prelude hiding (read, catch)
import Data.IORef

mySettings = setComplete noCompletion defaultSettings
main :: IO ()
main = do hSetBuffering stdout NoBuffering
          lb <- loadLawsFrom "default.laws"
          putStr "Using these axioms: \n"
	  showLawPriority lb
          lbRef <- newIORef lb
          runInputT mySettings (repl lbRef)

exit = putStrLn "Bye." >> exitWith ExitSuccess

repl :: IORef LawBank -> InputT IO ()
repl lbRef = do cmd <- read
                lift ((readIORef lbRef >>= eval cmd) `catch` (\(SomeEPLTException e) -> print e)) >> repl lbRef
    where read = do m <- getInputLine "> "
                    case m of
                      Nothing -> return Quit
                      Just [] -> return Nop
                      Just l ->  lift $ parseCmd "<interactive>" l `catch` (\(SomeEPLTException e) -> print e >> return Nop)
          eval c lb =
              case c of
                Quit -> exit
                LoadLaws fp -> do putStr ("Loading laws from " ++ fp ++ "...")
                                  lb' <- loadLawsFrom fp
                                  writeIORef lbRef lb'
                                  putStrLn "Done."
                ShowLaws -> showLawPriority lb
                AddLaw l -> do let lb' = addLaw l lb
                               writeIORef lbRef lb'
                               putStrLn "Law added."
                ProveAuto f -> (do putStr "Checking..."
                                   modelCheck f
                                   putStrLn "Formula is a tautology. Proving..."
                                   (p, st) <- prover lb f
                                   print p >> print st) `catch` (\UserInterrupt -> putStrLn "Proof interrupted.")
                Prove f -> putStrLn "Unimplemented."
                Nop -> return ()

