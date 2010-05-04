module Prover where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List
import Control.Monad.Logic.Class
import Control.Monad.Logic
import Debug.Trace

type Law = (Formula, Formula)

type HState = Int

type Subst = [(String, Formula)]
(|->) :: String -> Formula -> Subst
v |-> f = [(v,f)]

match :: (MonadPlus mp) => Formula -> Formula -> mp Subst
match (Var p) f = return (p |-> f)
match FFalse FFalse = return []
match FFalse _ = mzero
match FTrue FTrue = return []
match FTrue _ = mzero
match (Not f) (Not g) = match f g
match (f1 :& f2) (f3 :& f4) = do 	m1 <- match f1 f3
					m2 <- match f2 f4
					return (m1 ++ m2)
match (f1 :| f2) (f3 :| f4) = do 	m1 <- match f1 f3
					m2 <- match f2 f4
					return (m1 ++ m2)
match (f1 :== f2) (f3 :== f4) = do 	m1 <- match f1 f3
					m2 <- match f2 f4
					return (m1 ++ m2)
match (f1 :=> f2) (f3 :=> f4) = do 	m1 <- match f1 f3
					m2 <- match f2 f4
					return (m1 ++ m2)
match (f1 :<= f2) (f3 :<= f4) = do 	m1 <- match f1 f3
					m2 <- match f2 f4
					return (m1 ++ m2)
match (_ :& _ ) _ = mzero
match (_ :| _ ) _ = mzero
match (_ :== _ ) _ = mzero
match (_ :=> _ ) _ = mzero
match (_ :<= _ ) _ = mzero
match _ _ = mzero

unify :: (Functor m, MonadPlus m) => Subst -> m Subst
unify [] = return []
unify ((x,y):xs) | (ls == [] || ls == [y]) = fmap ((x,y):) (unify xs)
      		 | otherwise = mzero
    where ls = nub [b | (a,b) <- xs, a == x]

findLaws :: (Functor m, MonadPlus m) => Formula -> [Law] -> m (Law, Subst)
findLaws f = foldr findLaw mzero
    where findLaw l m = matcher l f `mplus` m
          matcher l@(lhs,_) f = fmap (\s -> (l,s)) (match lhs f >>= unify)


data DecoTree = DTVar String
     	      | DTTrue
	      | DTFalse
	      | DTNot [(Law, Subst)] DecoTree
	      | DTAnd [(Law,Subst)] DecoTree DecoTree
	      | DTOr [(Law, Subst)] DecoTree DecoTree
	      | DTEq [(Law, Subst)] DecoTree DecoTree
	      deriving (Eq, Show)

testLaws = [  
	      
	      -- Conmutatividad
	      (Var "a" :== Var "b", Var "b" :== Var "a") ,
	      (Var "a" :& Var "b", Var "b" :& Var "a") ,
	      (Var "a" :| Var "b", Var "b" :| Var "a") ,

              -- Reflexividad
	      (Var "a" :== Var "a", FTrue) , 

--	      (FTrue :== FFalse, FFalse),
--	      (FTrue :== FTrue, FTrue),

--	      (Not FTrue, FFalse),
--	      (Not FFalse, FTrue),
	      
	      -- Elementos neutros
	      (Var "a" :== FTrue, Var "a"),
	      (Var "a" :& FTrue , Var "a"),
	      (Var "a" :| FTrue, FTrue),
	      
	      -- Idempotencia
	      (Var "a" :& Var "a", Var "a"),
	      (Var "a" :| Var "a", Var "a"),

	      
--	      (Var "a" :& FFalse, FFalse),

--	      (Var "a" :| Var "b", Not ((Not (Var "a")) :& (Not (Var "b")))),

              -- Asociatividad (faltarian agregar la inversas que a veces hacen falta)
	      (Var "a" :== (Var "b" :== Var "c"), (Var "a" :== Var "b") :== Var "c"),
	      (Var "a" :& (Var "b" :& Var "c"), (Var "a" :& Var "b") :& Var "c"),	      
	      (Var "a" :| (Var "b" :| Var "c"), (Var "a" :| Var "b") :| Var "c"),
	      
	      -- Regla dorada
	      ((Var "a" :& Var "b"), Var "a" :== (Var "b" :== (Var "a" :| Var "b"))), -- Beware!
	      ((Var "a" :& Var "b") :==  Var "a", Var "b" :== (Var "a" :| Var "b")),
	      ((Var "a" :& Var "b") :== (Var "a" :== Var "b"),(Var "a" :| Var "b")),  
	      ((Var "a" :| Var "b"), Var "a" :== (Var "b" :== (Var "a" :& Var "b"))), -- Beware!
	      ((Var "a" :| Var "b") :==  Var "a", Var "b" :== (Var "a" :& Var "b")),
	      ((Var "a" :| Var "b") :== (Var "a" :== Var "b"),(Var "a" :& Var "b")), 
	      
	      -- Distributividad 
	      (Var "a" :| (Var "b" :== Var "c"), (Var "a" :| Var "b") :== (Var "a" :| Var "c")),	      
	      (Var "a" :| ( (Var "b" :== Var "c") :== (Var "a" :| Var "b") ), (Var "a" :| Var "c")),
	      
	      (Var "a" :& (Var "b" :== Var "c"), ((Var "a" :& Var "b") :== (Var "a" :& Var "c")) :== Var "p"),
	      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") ), (Var "a" :& Var "c") :== Var "p"),
	      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") :== (Var "a" :& Var "c")) , Var "p")


              --(Var "a" :& Var "b", Var "b" :& Var "a"),

	   ]
ordLaws x = case x of 
                      -- Reglas que SIEMPRE conviene seleccionar primero (reducen estructura)
                      (Var "a" :== Var "a", FTrue) 	     	    -> 0
		      (FTrue :== FFalse, FFalse)		    -> 0
		      (FTrue :== FTrue, FTrue)		    	    -> 0
		      (Not FTrue, FFalse)			    -> 0
		      (Not FFalse, FTrue)			    -> 0
		      (Var "a" :== FTrue, Var "a")		    -> 0
		      (Var "a" :& FTrue , Var "a")		    -> 0
		      (Var "a" :& Var "a", Var "a")		    -> 0
		      (Var "a" :| Var "a", Var "a")		    -> 0
		      (Var "a" :| FTrue, FTrue)		    	    -> 0
		      (Var "a" :& FFalse, FFalse)		    -> 0
		      
		      -- Reglas que conviene aplicar en 2do lugar
		      -- Golden rule
		      ((Var "a" :& Var "b"), Var "a" :== (Var "b" :== (Var "a" :| Var "b"))) -> 10
		      ((Var "a" :& Var "b") :==  Var "a", Var "b" :== (Var "a" :| Var "b")) -> 1
		      ((Var "a" :& Var "b") :== (Var "a" :== Var "b"),(Var "a" :| Var "b")) -> 1
		      ((Var "a" :| Var "b"), Var "a" :== (Var "b" :== (Var "a" :& Var "b"))) -> 10
		      ((Var "a" :| Var "b") :==  Var "a", Var "b" :== (Var "a" :& Var "b")) -> 1
		      ((Var "a" :| Var "b") :== (Var "a" :== Var "b"),(Var "a" :& Var "b")) -> 1
		      
		      
		      -- Reglas que conviene aplicar en ultimo lugar
		      
		      -- Permutatividad
                      (Var "a" :== Var "b", Var "b" :== Var "a") -> 1
		      (Var "a" :& Var "b", Var "b" :& Var "a")   -> 1
		      (Var "a" :| Var "b", Var "b" :| Var "a")   -> 1
		      
		      --(Var "a" :| Var "b", Not ((Not (Var "a")) :& (Not (Var "b")))) -> 2
		      
		      -- Asociatividad
		      (Var "a" :== (Var "b" :== Var "c"), (Var "a" :== Var "b") :== Var "c") -> 1
		      (Var "a" :& (Var "b" :& Var "c"), (Var "a" :& Var "b") :& Var "c") -> 1
		      (Var "a" :| (Var "b" :| Var "c"), (Var "a" :| Var "b") :| Var "c") -> 1
		      
		      -- Distributidad
		      (Var "a" :| (Var "b" :== Var "c"), (Var "a" :| Var "b") :== (Var "a" :| Var "c")) ->  1
		      (Var "a" :| ( (Var "b" :== Var "c") :== (Var "a" :| Var "b") ) , (Var "a" :| Var "c")) -> 1
		      
		      
		      (Var "a" :& (Var "b" :== Var "c"), ((Var "a" :& Var "b") :== (Var "a" :& Var "c")) :== Var "p") -> 1
		      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") ), (Var "a" :& Var "c") :== Var "p")-> 1  
		      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") :== (Var "a" :& Var "c") ) , Var "p")-> 1
		      


testFormula = ( (Var "p" :| (Var "p" :& Var "q")) :== Var "p") 
testFormulb = ( (Var "p" :& (Var "p" :| Var "q")) :== Var "p") 
testFormulc = ( (Var "p" :| (Var "q" :| Var "r") ) :== ( (Var "p" :| Var "q") :| (Var "p" :| Var "r") ) )
testFormuld = ( (Var "p" :& Var "q") :== (Var "p" :| Var "q") :& Var "p" :& Var "q" ) 

-- Deco functions!

decoTree :: Formula -> [Law] -> DecoTree
decoTree f ls = 
	 case f of
	      Var x -> DTVar x
	      FTrue -> DTTrue
	      FFalse -> DTFalse
	      Not f -> DTNot (findLaws f ls) (decoTree f ls)
	      f1 :& f2 -> DTAnd (findLaws f ls) (decoTree f1 ls) (decoTree f2 ls)
	      f1 :| f2 -> DTOr (findLaws f ls) (decoTree f1 ls) (decoTree f2 ls)
	      f1 :== f2 -> DTEq (findLaws f ls) (decoTree f1 ls) (decoTree f2 ls)

flatdeco f =   
   case f of
        DTVar x -> []
        DTTrue  -> []
	DTFalse -> []
	DTNot ss t -> ss:(flatdeco t)
	DTAnd ss t1 t2 -> ss : (flatdeco t1)++(flatdeco t2)
	DTOr ss t1 t2  -> ss : (flatdeco t1)++(flatdeco t2)
	DTEq ss t1 t2  -> ss : (flatdeco t1)++(flatdeco t2)

-- 

-- Deco Fusion
enumLaws :: (Functor m, MonadPlus m) => [Law] -> Formula -> m (Law, Subst)
enumLaws ls f =
    case f of
      Var x -> mzero
      FTrue -> mzero
      FFalse -> mzero
      Not f -> findLaws f ls `mplus` enumLaws ls f
      f1 :& f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2
      f1 :| f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2
      f1 :== f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2

--

applyl :: Formula -> (Law, Subst) -> Formula
applyl f ((lf, rf), s) = replace lf' rf' f
                         where lf' = substitute lf s
			       rf' = substitute rf s

substitute :: Formula -> Subst -> Formula
substitute f = foldr (\(s,d) -> replace (Var s) d ) f 

-- mkcomp_ rewritted!

type SComps = [(Law,Subst)]

-- mkcomp : toma una fórmula y devuelve la lista de computaciones suspendidas de las reescrituras posibles
mkcomp :: Formula -> SComps 
--mkcomp f = concat $ flatdeco $ decoTree f testLaws
mkcomp = observeAll . enumLaws testLaws -- faster!

interleaveCat :: [[a]] -> [a]
interleaveCat = foldr interleave []

interleaveMap :: (a -> [a]) -> [a] -> [a]
interleaveMap f xs  = interleaveCat $ map f xs

{-
step2 :: Int -> [Formula] -> Formula -> [Formula] 
step2 e ac f = a ++ (concat (map (step2 e' (a++ac)) a))
               where (c,e') = heuristica e f (mkcomp f)
	             a = filter (\ x -> not (elem x ac)) $ nub c
-}				 
 
-- Just testing... 
 
step3 :: [[Formula]] -> [Formula] -> [Formula]
step3 []           acc = []
step3 ([]:fss)     acc = step3 fss acc
step3 ((f:fs):fss) acc = nfs ++ (step3 (fss++[fs,nfs]) (nfs++acc))
                            where (fs',_) = heuristica 1 f (mkcomp f)
			          nfs = filter (\ x -> not (elem x acc)) $ nub fs'

--step :: Int -> Formula -> [Formula]
--step e ac f = a `mplus` interleaveMap (step e' (a++ac)) a
--            where (c,e') = heuristica_id e f (mkcomp f)
--		  a = filter (\ x -> not (elem x ac)) $ nub c 

heuristica_id :: HState -> Formula -> SComps -> ([Formula], HState)
heuristica_id e f fs = (mapplyl fs, e)
                       where mapplyl = map (applyl f) -- Force the suspended computations

heuristica :: HState -> Formula -> SComps -> ([Formula], HState) -- La heurística fuera las computaciones cuando lo necesite
heuristica e f fs = ( fs'', e)  
                    where fs' = sortBy (\x y-> compare (ordLaws (fst x)) (ordLaws (fst y))) fs -- Better rule
		          fs'' = sortBy (\x y -> compare (size x) (size y)) $  map (applyl f) fs' -- Most reduced branch


t y =  sum $ map (size.snd) $ snd y

			  
{-
heuristica_0 :: HState -> Formula -> [Formula] -> ([Formula], HState)
heuristica_0 e _ fs = ((sortBy (\x y -> compare (size x) (size y)) xs)++ys, e)  
                      where (xs,ys) = partition (\f -> size f <= 2) fs
-}

heuristica_1 :: HState -> Formula -> SComps -> ([Formula], HState)
heuristica_1 e f fs = (fs'', e+1)
		      where fs' = map (applyl f) fs
		            fs'' = (sortBy (\x y -> if (e<5) then compare (size y) (size x) else compare (size x) (size y)) fs')
{-
heuristica_2 :: HState -> Formula -> [Formula] -> ([Formula], HState)
heuristica_2 e f fs = x where x = (sortBy (\x y -> compare (comparef f x) (comparef f x)) fs, e)
-}

size :: Formula -> Int
size FTrue = 0
size FFalse = 0
size (Var _) = 1
size (a :& b) = max (size a) (size b) + 1
size (a :| b) = max (size a) (size b) + 1
size (a :== b) = max (size a) (size b) + 1
size (Not f) = size f + 1

-- Not used (yet)

comparef :: Formula -> Formula -> Int
comparef FTrue FTrue = 0
comparef FFalse FFalse = 0

comparef FTrue FFalse = 1
comparef FFalse FTrue  = 1

comparef FTrue y = size y 
comparef FFalse y = size y

comparef x FTrue = size x
comparef x FFalse = size x

comparef (Var x) (Var y) = if (x==y) then 0 else 1

comparef (Var _) y = size y
comparef x (Var _) = size x

comparef (x1 :& x2) (y1 :& y2) =  (comparef x1 y1) + (comparef x2 y2)
comparef (x1 :| x2) (y1 :| y2) =  (comparef x1 y1) + (comparef x2 y2)
comparef (x1 :== x2) (y1 :== y2) =  (comparef x1 y1) + (comparef x2 y2)

comparef x y = size x + size y