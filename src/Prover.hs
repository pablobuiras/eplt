module Prover where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List

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


g :: Maybe (Law, Subst) -> Maybe (Law, Subst)
g Nothing = Nothing
g (Just (x,y)) | unify y == Nothing = Nothing
       	       | otherwise  	    = Just (x,y)


data DecoTree = DTVar String
     	      | DTTrue
	      | DTFalse
	      | DTAnd [(Law,Subst)] DecoTree DecoTree
	      | DTOr [(Law, Subst)] DecoTree DecoTree
	      deriving (Eq, Show)

testLaws = [(Var "a" :& Var "a", Var "a"), (Var "a" :& Var "b", Var "b" :& Var "a"), ((Var "a" :& Var "b") :& Var "c",  (Var "a" :& (Var "b" :& Var "c"))) ]
testFormula = ((Var "q" :& Var "q") :& Var "p", FTrue) 


decoTree :: Formula -> [Law] -> DecoTree
decoTree f ls = 
	 case f of
	      Var x -> DTVar x
	      FTrue -> DTTrue
	      FFalse -> DTFalse
	      f1 :& f2 -> DTAnd (findLaws f ls) (decoTree f1 ls) (decoTree f2 ls)
	      f1 :| f2 -> DTOr (findLaws f ls) (decoTree f1 ls) (decoTree f2 ls)

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
       DTAnd l t1 t2 -> (listToMaybe l) `mplus` (h1 t1) `mplus` (h1 t2)
       DTOr  l t1 t2 -> (listToMaybe l) `mplus` (h1 t1) `mplus` (h1 t2)


h2 :: DecoTree -> DecoTree

h2 f =  
   case f of
        DTVar x -> DTVar x
        DTTrue  -> DTTrue
	DTFalse -> DTFalse
	DTAnd []     t1 t2 -> DTAnd [] (h2 t1) (h2 t2) 
	DTAnd (s:ss) t1 t2 -> DTAnd ss t1 t2
	DTOr []     t1 t2 -> DTOr [] (h2 t1) (h2 t2)
	DTOr (s:ss) t1 t2 -> DTOr ss t1 t2

h dt = (h1 dt, h2 dt) -- Not so fast.


applyl :: Formula -> (Law, Subst) -> Formula
applyl f ((lf, rf), s) = replace lf' rf' f
                         where lf' = substitute lf s
			       rf' = substitute rf s

substitute :: Formula -> Subst -> Formula
substitute f = foldr (\(s,d) -> replace (Var s) d ) f 

mkcomp_ :: DecoTree -> DecoTree -> (Formula,Formula) -> [(Formula,Formula)]
mkcomp_  ldt rdt (lf,rf) =
         case (h ldt) of
	      (Nothing, _)  -> []
	      (Just s, ldt') -> ((applyl lf s),rf):(mkcomp_ rdt ldt' (rf,lf)) -- I'm swapping the formula and the decotrees

mkcomp :: (Formula,Formula) -> [(Formula,Formula)]
mkcomp f@(lf,rf) = mkcomp_ (decoTree lf testLaws) (decoTree rf testLaws) f


step :: [[(Formula,Formula)]] -> [(Formula,Formula)]
step []          = []
step ([]:rs)      =  step rs
step ((f:fs):rs)  =  f:( step ( (mkcomp f):rs++[fs]) )

--step_ :: DecoTree -> Maybe Formula
--step_ 
