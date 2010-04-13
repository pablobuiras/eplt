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

testLaws = [(Var "p" :& Var "p", Var "p")]

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

h :: DecoTree -> Maybe (Law, Subst)
h f =
  case f of
       DTVar x -> Nothing
       DTTrue  -> Nothing
       DTFalse -> Nothing
       DTAnd l t1 t2 -> (listToMaybe l) `mplus` (h t1) `mplus` (h t2)
       DTOr  l t1 t2 -> (listToMaybe l) `mplus` (h t1) `mplus` (h t2)



--step_ :: DecoTree -> Maybe Formula
--step_ 