module Laws where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List
import Subst

type Law = (Formula, Formula)
type LawBank = [Law]

type SComp = (Law,Subst)
type SComps = [SComp]

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
	      
	      (Var "a" :& (Var "b" :== Var "c"), ((Var "a" :& Var "b") :== (Var "a" :& Var "c")) :== Var "a"),
	      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") ), (Var "a" :& Var "c") :== Var "a"),
	      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") :== (Var "a" :& Var "c")) , Var "a")


              --(Var "a" :& Var "b", Var "b" :& Var "a"),

	   ]
{-ordLaws x = case x of 
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
		      
		      
		      (Var "a" :& (Var "b" :== Var "c"), ((Var "a" :& Var "b") :== (Var "a" :& Var "c")) :== Var "a") -> 1
		      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") ), (Var "a" :& Var "c") :== Var "a")-> 1  
		      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") :== (Var "a" :& Var "c") ) , Var "a")-> 1
		      
-}

testFormula = ( (Var "p" :| (Var "p" :& Var "q")) :== Var "p") 
testFormulb = ( (Var "p" :& (Var "p" :| Var "q")) :== Var "p") 
testFormulc = ( (Var "p" :| (Var "q" :| Var "r") ) :== ( (Var "p" :| Var "q") :| (Var "p" :| Var "r") ) )
testFormuld = ( (Var "p" :& Var "q") :== (Var "p" :| Var "q") :& Var "p" :& Var "q" ) 

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


applyl :: Formula -> (Law, Subst) -> Formula
applyl f ((lf, rf), s) = replace lf' rf' f
                         where lf' = substitute lf s
			       rf' = substitute rf s

substitute :: Formula -> Subst -> Formula
substitute f = foldr (\(s,d) -> replace (Var s) d ) f 

genLawPriority :: [Law] -> [(Law, Int)]
genLawPriority ls = zip ls $ map ( \ (l,r) -> ((size r) - (size l))) ls 

testLawPriority = genLawPriority testLaws

showLawPriority :: LawBank -> IO ()
showLawPriority lb = do  putStr $ (pr "Law") ++ "\t Priority\n"
                         mapM_ ( \ (l,p) -> putStr (pr (show l) ++ "\t   "++ show p ++ "\n")) (genLawPriority lb)

pr s = s ++ take w (repeat ' ')
        where w = 40 - length s

ordGenericLaws lb = fromJust . flip lookup (genLawPriority lb)

-- to Heuristics.hs -->

size :: Formula -> Int
size FTrue = 0
size FFalse = 0
size (Var _) = 1
size (a :& b) = max (size a) (size b) + 1
size (a :| b) = max (size a) (size b) + 1
size (a :== b) = max (size a) (size b) + 1
size (Not f) = size f + 1
