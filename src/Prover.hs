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

expand :: Deriv -> Prover Deriv
expand d = let g = goal d
           in do incNodes
                 applyDerivH d $ fmap (derivStep d g) 
                                      (do lb <- constrainLaws d getLawBank
                                          applyLawH d (enumLaws lb g))

allit :: Deriv -> Prover Deriv
allit d = return d `mplus` (expand d >>- prune >>- allit)

pair :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
pair f g ~(x,y) = (f x, g y)


initState f =  PS { expanded = 0, depth = 0, visited = [f] }
initEnv =  PE { lawBank = undefined, heuristics = testH }

--testProver m = runProver initState initEnv $ m

reduceTo :: Formula -> Formula -> Prover Deriv
reduceTo lhs rhs = do d <- allit (startDeriv lhs)
                      guard (goal d == rhs)
                      return d

prove :: LawBank -> Formula -> Maybe (Deriv, ProverState)
prove lb f = wrapMaybe $ runProver (initState f) (initEnv { lawBank = lb }) (toplevel f)
    where toplevel f = once $ f `reduceTo` FTrue
          wrapMaybe ([],_) = Nothing
          wrapMaybe (xs,s) = Just (head xs, s)


-- IO Interface
data ProofNotFoundException = ProofNotFound
                            deriving Typeable

instance Show ProofNotFoundException where
    show ProofNotFound = "Formula does not follow from current axioms."

instance Exception ProofNotFoundException where
    toException = epltExceptionToException
    fromException = epltExceptionFromException

prover :: LawBank -> Formula -> IO (Deriv, ProverState)
prover lb f = maybe (throwIO ProofNotFound) return $ prove lb f


readInt :: String -> Int
readInt = read

showPosLaws :: LawBank -> Formula -> IO ([(Int,(Law,Subst))])
showPosLaws lb f = do mapM_ s pl
                      return pl
                        where pl = zip [1 ..] (observeAll (enumLaws lb f))
		              s (n,(l,s))  = putStrLn (pr (showLaw l) ++ " with: "++pr (showSubts s)++" -> "++show n)   

-- to Heuristics.hs -->
-- Trivial Heuristics
idH :: Heuristics
idH = (h1, h2, h3)
    where h1 _ _ lb = lb
          h2 _ _ ls = ls
          h3 _ _ cs = cs

-- Test Heuristics
testH :: Heuristics
testH = (h1, h2, h3)
    where h1 (PS { visited = fs}) _ lb = if (unit fs) then lb else lb { laws = filter ((<1) . ordGenericLaws) (laws lb) } -- he1
          h2 _ _ ls = ls
          h3 _ _ = sortBy (comparing (fsize . goal))
	  unit (_:[]) = True
	  unit _      = False
