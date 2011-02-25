{-# LANGUAGE DeriveDataTypeable #-}
module Prover where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad.Logic.Class
import Control.Monad.Logic
import ProverMonad
import Debug.Trace
import Laws --hiding (expand)
import Subst
import Deriv

import Data.Typeable
import Control.Exception
import Exceptions

expand :: Deriv -> Prover Deriv Deriv
expand d = let g = goal d
           in do incNodes
                 applyDerivH d $ fmap (derivStep d g)
                                      (do lb <- constrainLaws d getLawBank
                                          applyLawH d (enumLaws lb (mkZFormula g)))

allit :: Deriv -> Prover Deriv Deriv
allit d = return d `mplus` (expand d >>- prune >>- allit)

pair :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
pair f g ~(x,y) = (f x, g y)

initState f =  PS { expanded = 0, depth = 0, visited = [f] }
initEnv =  PE { lawBank = undefined, heuristics = idH }

--testProver m = runProver initState initEnv $ m

reduceTo :: Formula -> Formula -> Prover Deriv Deriv
reduceTo lhs rhs = do d <- allit (startDeriv lhs)
                      guard (goal d == rhs)
                      return d

prove :: LawBank -> Formula -> Either ProofException (Deriv, ProverState)
prove lb f = wrap $ runProver (initState f) (initEnv { lawBank = lb }) (toplevel f)
    where toplevel f = once $ f `reduceTo` FTrue
          wrap (Left e,_) = Left (ProofTooLong e)
          wrap (Right [],_) = Left ProofNotFound
          wrap (Right xs,s) = Right (head xs, s)


-- IO Interface
data ProofException = ProofNotFound
                    | ProofTooLong Deriv
                    deriving Typeable

instance Show ProofException where
    show ProofNotFound = "Formula does not follow from current axioms."
    show (ProofTooLong d) = "Proof could not be completed.\nPartial derivation:\n" ++ show d

instance Exception ProofException where
    toException = epltExceptionToException
    fromException = epltExceptionFromException

prover :: LawBank -> Formula -> IO (Deriv, ProverState)
prover lb f = either throwIO return $ prove lb (normalize f)


readInt :: String -> Int
readInt = read

showPosLaws :: LawBank -> Formula -> IO ([(Int,(Law,Subst, ZFormula))])
showPosLaws lb f = do mapM_ s pl
                      return pl
                        where pl = zip [1 ..] (observeAll (enumLaws lb (mkZFormula f)))
		              s (n,(l,s, _))  = putStrLn (pr (showLaw l) ++ " with: "++pr (showSubts s)++" -> "++show n)   

-- to Heuristics.hs -->
-- Trivial Heuristics
idH :: Heuristics
idH = (h1, h2, h3)
    where h1 (PS { visited = fs}) _ lb = if (unit fs) then lb else lb { laws = filter ((<=2) . ordGenericLaws) (laws lb) }
          h2 _ _ = id
          h3 _ _ = sortBy (comparing (fsize . goal))
          unit (_:[]) = True
	  unit _      = False


-- Test Heuristics
--testH :: Heuristics
--testH = (h1, h2, h3)
--    where h1 (PS { visited = fs}) _ lb = if (unit fs) then lb else lb { laws = filter ((<1) . ordGenericLaws) (laws lb) } -- he1
--          h2 _ _ ls = ls
--          h3 _ _ = sortBy (comparing (fsize . goal))
--	  unit (_:[]) = True
--	  unit _      = False
