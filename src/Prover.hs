module Prover where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List
import Control.Monad.Logic.Class
import Debug.Trace

type Law = (Formula, Formula)

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

findLaws :: Formula -> [Law] -> [(Law,Subst)]
findLaws f ls = let findLaw f l@(lhs,_) = fmap (\s -> (l,s)) (match lhs f)
	      	in catMaybes $ map g $ map (findLaw f) ls

t f x = trace (show x) (f x)

g :: Maybe (Law, Subst) -> Maybe (Law, Subst)
g Nothing = Nothing
g (Just (x,y)) | unify y == Nothing = Nothing
       	       | otherwise  	    = Just (x,y)


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
	      ((Var "a" :& Var "b"), Var "a" :== (Var "b" :== (Var "a" :| Var "b"))),
	      ((Var "a" :& Var "b") :==  Var "a", Var "b" :== (Var "a" :| Var "b")),
	      ((Var "a" :& Var "b") :== (Var "a" :== Var "b"),(Var "a" :| Var "b")),
	      ((Var "a" :| Var "b"), Var "a" :== (Var "b" :== (Var "a" :& Var "b"))),
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
		      ((Var "a" :& Var "b"), Var "a" :== (Var "b" :== (Var "a" :| Var "b"))) -> 1
		      ((Var "a" :& Var "b") :==  Var "a", Var "b" :== (Var "a" :| Var "b")) -> 1
		      ((Var "a" :& Var "b") :== (Var "a" :== Var "b"),(Var "a" :| Var "b")) -> 1
		      ((Var "a" :| Var "b"), Var "a" :== (Var "b" :== (Var "a" :& Var "b"))) -> 1
		      ((Var "a" :| Var "b") :==  Var "a", Var "b" :== (Var "a" :& Var "b")) -> 1
		      ((Var "a" :| Var "b") :== (Var "a" :== Var "b"),(Var "a" :& Var "b")) -> 1
		      
		      
		      -- Reglas que conviene aplicar en ultimo lugar
		      
		      -- Permutatividad
                      (Var "a" :== Var "b", Var "b" :== Var "a") -> 2
		      (Var "a" :& Var "b", Var "b" :& Var "a")   -> 2
		      (Var "a" :| Var "b", Var "b" :| Var "a")   -> 2
		      
		      --(Var "a" :| Var "b", Not ((Not (Var "a")) :& (Not (Var "b")))) -> 2
		      
		      -- Asociatividad
		      (Var "a" :== (Var "b" :== Var "c"), (Var "a" :== Var "b") :== Var "c") -> 2
		      (Var "a" :& (Var "b" :& Var "c"), (Var "a" :& Var "b") :& Var "c") -> 2
		      (Var "a" :| (Var "b" :| Var "c"), (Var "a" :| Var "b") :| Var "c") -> 2
		      
		      -- Distributidad
		      (Var "a" :| (Var "b" :== Var "c"), (Var "a" :| Var "b") :== (Var "a" :| Var "c")) ->  1	      
		      (Var "a" :| ( (Var "b" :== Var "c") :== (Var "a" :| Var "b") ) , (Var "a" :| Var "c")) -> 0
		      
		      
		      (Var "a" :& (Var "b" :== Var "c"), ((Var "a" :& Var "b") :== (Var "a" :& Var "c")) :== Var "p") -> 1  
		      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") ), (Var "a" :& Var "c") :== Var "p")-> 0  
		      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") :== (Var "a" :& Var "c") ) , Var "p")-> 0
		      


testFormula = ( (Var "p" :| (Var "p" :& Var "q")) :== Var "p") 
testFormulb = ( (Var "p" :& (Var "p" :| Var "q")) :== Var "p") 
--testFormulc = ( (Var "p" :& (Var "q" :& Var "r") ) :== ( (Var "p" :& Var "q") :& (Var "p" :& Var "r") ) )


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

unify :: Subst -> Maybe Subst
unify [] = Just []
unify l@((x,y):xs) | (ls == [] || ls == [y]) && (unify xs == Just xs) = Just l
      		   | otherwise = Nothing
      		   where ls = nub [b | (a,b) <- xs, a == x]

h1 :: DecoTree -> Maybe (Law, Subst)
h1 f =
  case f of
       DTVar x -> Nothing
       DTTrue  -> Nothing
       DTFalse -> Nothing
       DTNot l t -> (listToMaybe l) `mplus` (h1 t)
       DTAnd l t1 t2 -> (listToMaybe l) `mplus` (h1 t1) `mplus` (h1 t2)
       DTOr  l t1 t2 -> (listToMaybe l) `mplus` (h1 t1) `mplus` (h1 t2)
       DTEq  l t1 t2 -> (listToMaybe l) `mplus` (h1 t1) `mplus` (h1 t2)


h2 :: DecoTree -> DecoTree

h2 f =  
   case f of
        DTVar x -> DTVar x
        DTTrue  -> DTTrue
	DTFalse -> DTFalse
	DTNot [] t -> DTNot [] (h2 t)
	DTNot (x:xs) t -> DTNot xs t
	DTAnd []     t1 t2 -> DTAnd [] (h2 t1) (h2 t2) 
	DTAnd (s:ss) t1 t2 -> DTAnd ss t1 t2
	DTOr []     t1 t2 -> DTOr [] (h2 t1) (h2 t2)
	DTOr (s:ss) t1 t2 -> DTOr ss t1 t2
	DTEq []     t1 t2 -> DTEq [] (h2 t1) (h2 t2)
	DTEq (s:ss) t1 t2 -> DTEq ss t1 t2

--h dt = (h1 dt, h2 dt) -- Not so fast.

h dt = if (null fdt) then (Nothing, dt) else (Just (l,s), deldeco dt (l,s))
       where fdt = concat $ flatdeco dt
             (m,l,s) = findmin (map ( \ (l_,s_) -> (ordLaws l_, l_ , s_) )  fdt) 

flatdeco f =   
   case f of
        DTVar x -> []
        DTTrue  -> []
	DTFalse -> []
	DTNot ss t -> ss:(flatdeco t)
	DTAnd ss t1 t2 -> ss : (flatdeco t1)++(flatdeco t2)
	DTOr ss t1 t2  -> ss : (flatdeco t1)++(flatdeco t2)
	DTEq ss t1 t2  -> ss : (flatdeco t1)++(flatdeco t2)

deldeco dt e = -- Puede optimizarse un poco
   case dt of
        DTVar x -> DTVar x 
        DTTrue  -> DTTrue
	DTFalse -> DTFalse
	DTNot ss t -> DTNot (delete e ss) (deldeco t e)
	DTAnd ss t1 t2 -> DTAnd (delete e ss) (deldeco t1 e) (deldeco t2 e)
	DTOr ss t1 t2  -> DTOr (delete e ss) (deldeco t1 e) (deldeco t2 e)
	DTEq ss t1 t2  -> DTEq (delete e ss) (deldeco t1 e) (deldeco t2 e)

findmin = foldr1 ( \ (n1, l1, s1) (n2, l2, s2) -> if (n1<n2) then (n1, l1, s1) else (n2, l2, s2))

applyl :: Formula -> (Law, Subst) -> Formula
applyl f ((lf, rf), s) = replace lf' rf' f
                         where lf' = substitute lf s
			       rf' = substitute rf s

substitute :: Formula -> Subst -> Formula
substitute f = foldr (\(s,d) -> replace (Var s) d ) f 

mkcomp_ :: DecoTree -> Formula -> [Formula]
mkcomp_  dt f =
         case (h dt) of
	      (Nothing, _)  -> []
	      (Just s, dt') -> (applyl f s):(mkcomp_ dt' f)

mkcomp :: Formula -> [Formula]
mkcomp f = x where x = mkcomp_ (decoTree f testLaws) f

interleaveCat :: [[a]] -> [a]
interleaveCat = foldr interleave []

interleaveMap :: (a -> [a]) -> [a] -> [a]
interleaveMap f xs  = interleaveCat $ map f xs

step :: Formula -> [Formula]
step f = c `interleave` interleaveMap step c
         where c = nub $ mkcomp f             -- Desacelera un poco el uso creciente de memoria
