module Main where

-- core modules
import Assistant (proofAssistant)
import Prover (prover)
import ModelBuilder (modelCheck)


import Formula.Pretty
import System.IO
import Control.Monad
import System.Console.Haskeline hiding (catch)
import Control.Monad.Trans
import Control.Exception
import System.Exit
import Exceptions
import Laws
import Configure.Load
import Configure.Options
import Commands
import Commands.Parser
import Prelude hiding (read, catch)
import Data.IORef
import System.CPUTime
import System.Console.GetOpt
import System.Environment

mySettings = setComplete noCompletion defaultSettings
main :: IO ()
main = do args <- getArgs
          hSetBuffering stdout NoBuffering
          let (actions, nonOptions, errors) = getOpt RequireOrder options args
          opts <- foldl (>>=) (return startOptions) actions
          lb <- optLaws opts
          putStr "Using these axioms: \n"
	  showLawPriority lb
          lbRef <- newIORef lb
          runInputT mySettings (repl lbRef opts)

exit = putStrLn "Bye." >> exitWith ExitSuccess

time :: IO a -> IO a
time m = do t1 <- getCPUTime
            a <- m
            t2 <- getCPUTime
            putStrLn ("Proof completed in " ++ show (fromIntegral (t2 - t1)/(10^12)) ++ " second(s).")
            return a

repl :: IORef LawBank -> Options -> InputT IO ()
repl lbRef opts = do cmd <- read
                     lift ((readIORef lbRef >>= eval cmd) `catch` (\(SomeEPLTException e) -> print e)) >> repl lbRef opts
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
                Reload -> eval (LoadLaws (fileName lb)) lb
                ShowLaws -> showLawPriority lb
                AddLaw l -> do let lb' = addLaw l lb
                               writeIORef lbRef lb'
                               putStrLn "Law added."
                ProveAuto f -> (do time (do putStr "Checking..."
                                            modelCheck f
                                            putStrLn "Formula is a tautology. Proving..."
                                            (p, st) <- prover lb f
                                            print p
                                            print st)
                                   eval (optPostQed opts f) lb) `catch` (\UserInterrupt -> putStrLn "Proof interrupted.")
                Prove f -> do m <- proofAssistant lb f
                              case m of
                                Nothing -> do putStrLn "Proof assistant stopped."
                                Just d -> do putStr "Proof completed: "
                                             print f
                                             print d
                                             eval (optPostQed opts f) lb
                Nop -> return ()

