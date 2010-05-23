module Laws where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Ord
import Subst

type Law = (Formula, Formula)
type LawBank = [(Law, Int)]

type SComp = (Law,Subst)
type SComps = [SComp]
     
testFormula = ( (Var "p" :| (Var "p" :& Var "q")) :== Var "p") 
testFormulb = ( (Var "p" :& (Var "p" :| Var "q")) :== Var "p") 
testFormulc = ( (Var "p" :| (Var "q" :| Var "r") ) :== ( (Var "p" :| Var "q") :| (Var "p" :| Var "r") ) )
testFormuld = ( (Var "p" :& Var "q") :== (Var "p" :| Var "q") :& Var "p" :& Var "q" ) 
testFormule = ( (Var "p" :| (Var "q" :& Var "r") ) :== ( (Var "p" :| Var "q") :& (Var "p" :| Var "r") ) )


testLaws = [  
	      
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
	      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") :== (Var "a" :& Var "c")) , Var "a"),

	      -- Conmutatividad
	      (Var "a" :& Var "b", Var "b" :& Var "a") ,
	      (Var "a" :| Var "b", Var "b" :| Var "a") ,
	      (Var "a" :== Var "b", Var "b" :== Var "a")

              --(Var "a" :& Var "b", Var "b" :& Var "a"),

	   ]

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
    where findLaw (l,_) m = matcher l f `mplus` m
          matcher l@(lhs,_) f = fmap (\s -> (l,s)) (match lhs f >>= unify)


applyl :: Formula -> (Law, Subst) -> Formula
applyl f ((lf, rf), s) = replace lf' rf' f
                         where lf' = substitute lf s
			       rf' = substitute rf s

substitute :: Formula -> Subst -> Formula
substitute f = foldr (\(s,d) -> replace (Var s) d ) f 

genLawPriority :: [Law] -> LawBank
genLawPriority ls = sortBy (comparing snd) $ zip ls $ map ( \ (l,r) -> ((size r) - (size l))) ls 

showLawPriority :: LawBank -> IO ()
showLawPriority lb = do  putStr $ (pr "Law") ++ "\t Priority\n"
                         mapM_ ( \ (l,p) -> putStr (pr (showLaw l) ++ "\t   "++ show p ++ "\n")) lb


-- TODO: make show instances of Law and Subts
showLaw (l,r) = show l ++ " <=> "++show r
showSubts ss = concat $ map ( \ (v,e) -> v++" => "++show e++" , ") ss 

pr s = s ++ take w (repeat ' ')
        where w = 40 - length s

ordGenericLaws = snd

addLaw :: Law -> LawBank -> LawBank
addLaw l lb = genLawPriority (l : map fst lb)

-- to Heuristics.hs -->

size :: Formula -> Int
size FTrue = 0
size FFalse = 0
size (Var _) = 1
size (a :& b) = max (size a) (size b) + 1
size (a :| b) = max (size a) (size b) + 1
size (a :== b) = max (size a) (size b) + 1
size (Not f) = size f + 1
