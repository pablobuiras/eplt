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

findLaws :: (Functor m, MonadPlus m) => Formula -> LawBank -> m (Law, Subst)
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


-- Deco Fusion
enumLaws :: (Functor m, MonadPlus m) => LawBank -> Formula -> m (Law, Subst)
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

type SComp = (Law,Subst)
type SComps = [SComp]
data ProverStatus = Status { formula :: Formula, lawbank :: LawBank, heuristics :: Heuristics, expanded :: Int } -- deriving Show
type Heuristics = ( ProverStatus -> LawBank, ProverStatus -> [(SComp,Formula)] -> [(SComp,Formula)])

-- to Law.hs ->
type LawBank = [Law] -- temporal
-- 

-- mkcomp : devuelve la lista de computaciones suspendidas de las reescrituras posibles
mkcomp :: ProverStatus -> SComps 
mkcomp ps = observeAll $ enumLaws (lawbank ps) (formula ps) -- faster!

h_id_1 :: ProverStatus -> LawBank
h_id_1 ps = lawbank ps

h_id_2 :: ProverStatus -> [(SComp,Formula)] -> [(SComp,Formula)]
h_id_2 _ ls = ls

h_id :: Heuristics
h_id = (h_id_1, h_id_2)

h_stable_1 :: ProverStatus -> LawBank
h_stable_1 ps = lawbank ps

h_stable_2 :: ProverStatus -> [(SComp,Formula)] -> [(SComp,Formula)]
h_stable_2 _ cs = sortBy ( \ (_,x) (_,y) -> compare (size x) (size y)) cs

h_stable :: Heuristics
h_stable = (h_stable_1, h_stable_2)


h_experimental_1 :: ProverStatus -> LawBank
h_experimental_1 (Status {expanded = n, lawbank = lb}) = if (n==0) then lb else []

h_experimental_2 :: ProverStatus -> [(SComp,Formula)] -> [(SComp,Formula)]
h_experimental_2 _ cs = sortBy ( \ (_,x) (_,y) -> compare (size x) (size y)) cs

h_experimental :: Heuristics
h_experimental = (h_experimental_1, h_experimental_2)


chgLawBank :: ProverStatus -> LawBank -> ProverStatus
chgLawBank ps new_lawbank = Status { formula = formula ps, lawbank = new_lawbank, heuristics = heuristics ps , expanded = expanded ps }

chgFormula :: ProverStatus -> Formula -> ProverStatus
chgFormula ps new_formula = Status { formula = new_formula, lawbank = lawbank ps, heuristics = heuristics ps, expanded = expanded ps }


inc :: ProverStatus -> ProverStatus
inc ps  = Status { formula = formula ps, lawbank = lawbank ps, heuristics = heuristics ps, expanded = expanded ps + 1 }



{-

interleaveCat :: [[a]] -> [a]
interleaveCat = foldr interleave []

interleaveMap :: (a -> [a]) -> [a] -> [a]
interleaveMap f xs  = interleaveCat $ map f xs


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

size :: Formula -> Int
size FTrue = 0
size FFalse = 0
size (Var _) = 1
size (a :& b) = max (size a) (size b) + 1
size (a :| b) = max (size a) (size b) + 1
size (a :== b) = max (size a) (size b) + 1
size (Not f) = size f + 1
