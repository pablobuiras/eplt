module Assistant where

import Commands
import Commands.Parser
import Formula
import Laws
import Subst
import Deriv
import Prover (prover)

import System.Console.Haskeline hiding (catch)
import Exceptions
import Control.Exception
import System.IO
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Logic
import Data.Maybe
import Debug.Trace
import Control.Monad.Maybe
import Prelude hiding (catch)
import Data.Char

repl :: LawBank -> Deriv -> InputT IO (Maybe Deriv)
repl lb d = do cmd <- read
      	       (lift (eval lb d cmd)) >>= maybe (return Nothing) (\d -> if qed d then return (Just d) else repl lb d)
               where    read = do m <- getInputLine "*> "
      	    	       	       	  case m of
		      	       	    Nothing -> return Leave
			      	    Just [] -> return NopAssistant
			      	    Just l -> lift $ parseAssistant "<assistant>" l `catch` (\(SomeEPLTException e) -> print e >> return NopAssistant)
			eval lb d c = runMaybeT $
			       	  case c of
	      	       	      	    Leave -> mzero
                                    Auto -> lift $ (do (p,_) <- prover lb (goal d)
                                                       return (appendDeriv d (tailDeriv p))) `catches` [Handler (\UserInterrupt -> putStrLn "Proof interrupted." >> return d),
                                                                                                        Handler (\(SomeEPLTException e) -> print e >> return d)]
                                    ShowDeriv -> lift (print d >> return d)
                                    Qed -> return d -- TODO: make this more useful
		     	      	    Use l -> lift $ case constrainLB l lb of
                                                       Nothing -> do putStrLn "No such law exists."
                                                                     return d
                                                       Just lb' ->
                                                           do let choices = observeAll $ enumLaws lb' (goal d)
                                                              m <- userChoice choices
                                                              maybe (return d) (chooseStep d (goal d)) m
		      	      	    List -> lift $ do putStrLn "Applicable laws:"
                                                      m <- userChoice laws
                                                      maybe (return d) (chooseStep d (goal d)) m
		      	      	    BT -> return (derivUnstep d)
		      	      	    NopAssistant -> return d
				    Goal -> do lift $ putGoal d
				    	       return d
			laws = observeAll $ enumLaws lb (goal d)

userChoice :: Show a => [a] -> IO (Maybe a)
userChoice [] = return Nothing
userChoice [l] = return (Just l)
userChoice ls = do mapM (\(i,s) -> putStrLn $ "rule " ++ show i ++ " : " ++ show s) $ zip [0..] ls
                   line <- runInputT defaultSettings $ getInputLine "Enter a number (anything else aborts): "
                   case line of
                     Nothing -> return Nothing
                     Just [] -> return Nothing
                     Just l -> do let n = read l :: Int
                                  return (if (all isDigit l && n < length ls) then Just (ls !! n) else Nothing)

showNumberedList :: Show a => [a] -> IO ()
showNumberedList l = do putStrLn "Applicable laws :"
	             	mapM (\(i,s) -> putStrLn $ "rule " ++ show i ++ " : " ++ show s) $ zip [0..] l
			return ()

putGoal :: Deriv -> IO ()
putGoal d = putStrLn $ "Goal : " ++ show (goal d)

chooseStep :: Deriv -> Formula -> (Law, Subst) -> IO Deriv
chooseStep d f l = do let d' = derivStep d f l
                      putGoal d'
                      return d'

proofAssistant lb f = runInputT defaultSettings $ outputStrLn "Starting proof by hand..." >> outputStrLn ("Goal : " ++ show f) >> repl lb (startDeriv f)