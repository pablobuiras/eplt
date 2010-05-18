{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Prover where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List
import Control.Monad.Trans
import Lazy
import Control.Monad.Sharing.Classes
import Control.Monad.Logic.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import ProverMonad
import Debug.Trace
import Laws
import Subst
import Proof
import Deriv

import Control.Monad.Logic
import Control.Monad.State


type Heuristics = ( ProverState -> LawBank, ProverState -> [(SComp,Formula)] -> [(SComp,Formula)])
type Answer = Lazy Prover Formula

{-
data MonadPlus m => Rose m a = Rose a (m (Rose m a))

build :: Formula -> Rose Prover Formula
build f = Rose f (fmap build (mstep f))

flatten :: Rose Prover Formula -> Prover Formula
flatten (Rose f mf) = return f `interleave` (mf >>- flatten)

doit :: Formula -> Answer
doit = flatten . build


instance Shareable (Lazy Prover) Formula where
    shareArgs = const return

instance Functor (Lazy Prover) where
    fmap f lm = lift (fmap f (runLazy lm))

instance MonadLogic (Lazy Prover) where
    msplit l = let m = runLazy l
               in lift (msplit m >>= \x -> case x of
	                                     Nothing -> return Nothing
					     Just (a,ma) -> return (Just (a, lift ma)))

instance MonadProver (Lazy Prover) Formula where
    incNodes = lift incNodes
    incDepth = lift incDepth
    prune = lift . prune
    applyAll f lm = let m = runLazy lm
                    in lift (applyAll f m)
    getLawBank = lift getLawBank

instance MonadWriter Proof (Lazy Prover) where
    tell = lift . tell
    listen lm = let m = runLazy lm
                in lift (listen m)
    pass lm = let m = runLazy lm
              in lift (pass m)
-}

enumLaws :: (Functor m, MonadLogic m) => LawBank -> Formula -> m (Law, Subst)
enumLaws ls f = case f of
                  Var x -> mzero
                  FTrue -> mzero
                  FFalse -> mzero
                  Not f -> findLaws f ls `mplus` enumLaws ls f
                  f1 :& f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2
                  f1 :| f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2
                  f1 :== f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2

{-
-- mkcomp : devuelve la lista de computaciones suspendidas de las reescrituras posibles
mkcomp :: Formula -> Lazy Prover SComp
mkcomp f = getLawBank >>= \lb -> incNodes >> enumLaws lb f



mstep :: Formula -> Answer
mstep f = do (l,s) <- mkcomp f
             tell $ proofstep f l s
             return (applyl f (l,s))

estep :: Answer -> Answer
estep a = do x <- a
             --prune x
	     --record x
             y <- mstep x
             return y
-}
{-
allit :: Answer -> Answer
allit a = do incDepth
             x <- return (a>>-mstep)
             x `mplus` (allit x)
-}

expand :: Deriv -> Prover Deriv
expand d = let g = goal d
           in incNodes >> fmap (derivStep d g) 
                               (getLawBank >>= \lb -> enumLaws lb g)

allit :: Deriv -> Prover Deriv
allit d = return d `mplus` (expand d >>- prune >>- allit)

pair :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
pair f g ~(x,y) = (f x, g y)

testProver m = let st = PS { expanded = 0, depth = 0, visited = [] }
               in runProver st testLaws $ m

prove :: Formula -> (Deriv, ProverState)
prove f = pair head id $ runProver st testLaws (toplevel f)
          where st = PS { expanded = 0, depth = 0, visited = [] } 
                toplevel f = once $ do d <- allit (startDeriv f)
                                       guard (qed d)
                                       return d

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

mplusCat :: [[a]] -> [a]
mplusCat = foldr mplus []

mplusMap :: (a -> [a]) -> [a] -> [a]
mplusMap f xs  = mplusCat $ map f xs


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
