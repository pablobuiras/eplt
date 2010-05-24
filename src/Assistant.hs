module Assistant where

import Commands
import Commands.Parser
import Formula
import Laws
import Subst
import Deriv

import System.Console.Haskeline hiding (catch)
import Exceptions
import System.IO
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Logic
import Data.Maybe
import Debug.Trace
import Control.Monad.Maybe

repl :: LawBank -> Deriv -> InputT IO ()
repl lb d = do cmd <- read
      	       (lift (eval lb d cmd)) >>= maybe (return ()) (repl lb)
               where    read = do m <- getInputLine "*> "
      	    	       	       	  case m of
		      	       	    Nothing -> return Leave
			      	    Just [] -> return NopAssistant
			      	    Just l -> lift $ parseAssistant "<assistant>" l
			eval lb d c = runMaybeT $
			       	  case c of
	      	       	      	    Leave -> mzero
				    Qed -> guard (not (qed d)) >> return d
		     	      	    Use i -> return $ derivStep d (goal d) (laws !! i)
		      	      	    List -> showNumberedList laws >> return d
		      	      	    BT -> return $ derivUnstep d
		      	      	    NopAssistant -> return d
				    Goal -> (lift $ putStrLn $ "Goal : " ++ show (goal d)) >> return d
			laws = observeAll $ enumLaws lb (goal d)

showNumberedList :: Show a => [a] -> MaybeT IO ()
showNumberedList l = do lift $ putStrLn "Applicable laws :"
	             	lift $ mapM (\(i,s) -> putStrLn $ "rule " ++ show i ++ " : " ++ show s) $ zip [0..] l
			return ()

proofAssistant lb f = runInputT defaultSettings $ repl lb (startDeriv f)