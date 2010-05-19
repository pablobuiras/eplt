{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Prover where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Maybe
import Data.List
import Control.Monad.Logic.Class
import ProverMonad
import Debug.Trace
import Laws
import Subst
import Deriv

enumLaws :: (Functor m, MonadLogic m) => LawBank -> Formula -> m (Law, Subst)
enumLaws ls f = case f of
                  Var x -> mzero
                  FTrue -> mzero
                  FFalse -> mzero
                  Not f -> findLaws f ls `mplus` enumLaws ls f
                  f1 :& f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2
                  f1 :| f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2
                  f1 :== f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2

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
initEnv =  PE { lawBank = testLaws, heuristics = testH }

--testProver m = runProver initState initEnv $ m

prove :: Formula -> (Deriv, ProverState)
prove f = pair head id $ runProver (initState f) initEnv (toplevel f)
          where toplevel f = once $ do d <- allit (startDeriv f)
                                       guard (qed d)
                                       return d

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
    where h1 (PS { visited = fs}) _ ls = if (unit fs) then ls else filter (\ l -> ordGenericLaws l < 1) ls-- he1
          h2 _ _ ls = ls
          h3 _ _ cs = sortBy ( \x y -> compare (size $ goal x) (size $ goal y)) cs
	  unit (_:[]) = True
	  unit _      = False

genLawPriority :: [Law] -> [(Law, Int)]
genLawPriority ls = zip ls $ map ( \ (l,r) -> ((size r) - (size l))) ls 

testLawPriority = genLawPriority testLaws

showLawPriority :: IO ()
showLawPriority = do  putStr $ (pr "Law") ++ "\t Priority\n"
                      mapM_ ( \ (l,p) -> putStr (pr (show l) ++ "\t   "++ show p ++ "\n")) testLawPriority  

pr s = s ++ take w (repeat ' ')
        where w = 40 - length s

ordGenericLaws = fromJust . flip lookup testLawPriority