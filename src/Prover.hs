{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Prover where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List
import Control.Monad.Trans
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
                                      (do lb <- getLawBank
                                          applyLawH d (constrainLaws d
                                                       (enumLaws lb g)))

allit :: Deriv -> Prover Deriv
allit d = return d `mplus` (expand d >>- prune >>- allit)

pair :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
pair f g ~(x,y) = (f x, g y)


initState =  PS { expanded = 0, depth = 0, visited = [] }
initEnv =  PE { lawBank = testLaws, heuristics = testH }

testProver m = runProver initState initEnv $ m

prove :: Formula -> (Deriv, ProverState)
prove f = pair head id $ runProver initState initEnv (toplevel f)
          where toplevel f = once $ do d <- allit (startDeriv f)
                                       guard (qed d)
                                       return d

-- Trivial Heuristics
idH :: Heuristics
idH = (h1, h2, h3)
    where h1 _ _ lb = lb
          h2 _ _ ls = ls
          h3 _ _ cs = cs

-- Test Heuristics
testH :: Heuristics
testH = (h1, h2, h3)
    where h1 (PS { depth = d}) _ = filter (\ l -> (d==0) || ordLaws l < 10)
          h2 _ _ ls = ls
          h3 _ _ cs = sortBy ( \x y -> compare (size $ goal x) (size $ goal y)) cs

-- Heuristics (old stuff)

{-
h_id_1 :: ProverState -> LawBank
h_id_1 ps = lawbank ps

h_id_2 :: ProverState -> [(SComp,Formula)] -> [(SComp,Formula)]
h_id_2 _ ls = ls

h_id :: Heuristics
h_id = (h_id_1, h_id_2)

h_stable_1 :: ProverState -> LawBank
h_stable_1 ps = lawbank ps

h_stable_2 :: ProverState -> [(SComp,Formula)] -> [(SComp,Formula)]
h_stable_2 _ cs = sortBy ( \ (_,x) (_,y) -> compare (size x) (size y)) cs

h_stable :: Heuristics
h_stable = (h_stable_1, h_stable_2)

 
h_experimental_1 :: ProverState -> LawBank
h_experimental_1 (Status {expanded = n, lawbank = lb}) = if (n==0) then lb else []

h_experimental_2 :: ProverState -> [(SComp,Formula)] -> [(SComp,Formula)]
h_experimental_2 _ cs = sortBy ( \ (_,x) (_,y) -> compare (size x) (size y)) cs

h_experimental :: Heuristics
h_experimental = (h_experimental_1, h_experimental_2)

-}

{-

-- Just testing... 
 
step :: [[Formula]] -> [Formula] -> HState -> [Formula]
step []           acc e = []
step ([]:fss)     acc e = step fss acc e
step ((f:fs):fss) acc e = nfs ++ (step (fss++[fs,nfs]) (nfs++acc) e')
                            where (fs',e') = heuristica_2 e f (mkcomp f)
			          nfs = filter (\ x -> not (elem x acc)) $ nub fs'

heuristica_id :: HState -> Formula -> SComps -> ([Formula], HState)
heuristica_id e f fs = (mapplyl fs, e)
                       where mapplyl = map (applyl f) -- Force the suspended computations

heuristica :: HState -> Formula -> SComps -> ([Formula], HState) -- La heurística fuera las computaciones cuando lo necesite
heuristica e f fs = ( fs'', e)  
                    where fs' = sortBy (\x y-> compare (ordLaws (fst x)) (ordLaws (fst y))) fs -- Better rule
		          fs'' = sortBy (\x y -> compare (size x) (size y)) $  map (applyl f) fs' -- Most reduced branch


t y =  sum $ map (size.snd) $ snd y



heuristica_1 :: HState -> Formula -> SComps -> ([Formula], HState)
heuristica_1 e f fs = (fs'', e+1)
		      where fs' = map (applyl f) fs
		            fs'' = (sortBy (\x y -> if (e<5) then compare (size y) (size x) else compare (size x) (size y)) fs')

heuristica_2 :: HState -> Formula -> SComps -> ([Formula], HState) -- "Magic" heuristic
heuristica_2 e f fs = (fs''', 1)
		      where fs' = if (e>0) then filter (\x -> (ordLaws (fst x)) < 10) fs else fs
		            fs'' = map (applyl f) fs'
 			    fs''' = (sortBy (\x y -> compare (size x) (size y)) fs'')
-}
