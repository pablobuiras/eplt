module Assistant where

import Commands
import Commands.Parser
import Formula
import Laws
import Subst
import Deriv

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

repl :: LawBank -> Deriv -> InputT IO ()
repl lb d = do cmd <- read
      	       (lift (eval lb d cmd)) >>= maybe (return ()) (repl lb)
               where    read = do m <- getInputLine "*> "
      	    	       	       	  case m of
		      	       	    Nothing -> return Leave
			      	    Just [] -> return NopAssistant
			      	    Just l -> lift $ parseAssistant "<assistant>" l `catch` (\(SomeEPLTException e) -> print e >> return NopAssistant)
			eval lb d c = runMaybeT $
			       	  case c of
	      	       	      	    Leave -> mzero
				    Qed -> guard (not (qed d)) >> return d
		     	      	    Use i -> do lift $ putGoal next
				    	     	return next
					where next = if i < length laws then derivStep d (goal d) (laws !! i) else d
		      	      	    List -> do lift $ showNumberedList laws
				    	       return d
		      	      	    BT -> return (derivUnstep d)
		      	      	    NopAssistant -> return d
				    Goal -> do lift $ putGoal d
				    	       return d
			laws = observeAll $ enumLaws lb (goal d)

showNumberedList :: Show a => [a] -> IO ()
showNumberedList l = do putStrLn "Applicable laws :"
	             	mapM (\(i,s) -> putStrLn $ "rule " ++ show i ++ " : " ++ show s) $ zip [0..] l
			return ()

putGoal :: Deriv -> IO ()
putGoal d = putStrLn $ "Goal : " ++ show (goal d)

proofAssistant lb f = runInputT defaultSettings $ outputStrLn "Starting proof by hand..." >> outputStrLn ("Goal : " ++ show f) >> repl lb (startDeriv f)