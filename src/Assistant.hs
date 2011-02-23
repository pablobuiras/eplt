module Assistant where

import Commands
import Commands.Parser
import Deriv
import Formula
import Laws
import Prover (prover)
import Subst

import Control.Exception
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Maybe
import Control.Monad.Trans
import Data.Char
import Data.Maybe
import Exceptions
import Prelude hiding (catch)
import System.Console.Haskeline hiding (catch)
import System.IO

newtype ZFProxy = ZFProxy { unZFProxy :: ZFormula }

instance Show ZFProxy where
  show (ZFProxy zf) = show (getFormula zf)

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
                                                           do let choices = observeAll $ enumLaws lb' (mkZFormula (goal d))
                                                              m <- userChoice (map (\(a,b,c) -> (a,b,ZFProxy c)) choices)
                                                              maybe (return d) (chooseStep d (goal d) . (\(a,b,c) -> (a,b,unZFProxy c))) m
		      	      	    List -> lift $ do putStrLn "Applicable laws:"
                                                      m <- userChoice (map (\(a,b,c) -> (a,b,ZFProxy c)) laws)
                                                      maybe (return d) (chooseStep d (goal d) . (\(a,b,c) -> (a,b,unZFProxy c))) m
		      	      	    BT -> lift $ case hasStepDeriv d of
				       	       	      True -> do putGoal d'
						      	      	 return d'
								 where d' = derivUnstep d
						      False -> do putStrLn "Cannot backtrace."
						      	       	  putGoal d
								  return d
		      	      	    NopAssistant -> return d
				    Goal -> do lift $ putGoal d
				    	       return d
			laws = observeAll $ enumLaws lb (mkZFormula (goal d))

userChoice :: Show a => [a] -> IO (Maybe a)
userChoice [] = return Nothing
userChoice [l] = return (Just l)
userChoice ls = do mapM (\(i,s) -> putStrLn $ "Rule " ++ show i ++ " : " ++ show s) $ zip [0..] ls
                   line <- runInputT defaultSettings $ getInputLine "Enter a number (anything else aborts): "
                   case line of
                     Nothing -> return Nothing
                     Just [] -> return Nothing
                     Just l -> do let n = read l :: Int
                                  return (if (all isDigit l && n < length ls) then Just (ls !! n) else Nothing)

showNumberedList :: Show a => [a] -> IO ()
showNumberedList l = do putStrLn "Applicable laws:"
	             	mapM (\(i,s) -> putStrLn $ "rule " ++ show i ++ " : " ++ show s) $ zip [0..] l
			return ()

putGoal :: Deriv -> IO ()
putGoal d = putStrLn $ "Goal : " ++ show (goal d)


chooseStep :: Deriv -> Formula -> (Law, Subst, ZFormula) -> IO Deriv
chooseStep d f l = do let d' = derivStep d f l
                      putGoal d'
                      return d'

proofAssistant lb f = runInputT defaultSettings $ outputStrLn "Starting proof by hand..." >> outputStrLn ("Goal : " ++ show f) >> repl lb (startDeriv f)
