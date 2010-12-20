module Formula where


import Data.List
import Maybe


data Formula = Var String
             | FTrue | FFalse
             | FAnd   [Formula]
             | FOr    [Formula]
	     | FEquiv [Formula]
             | Formula :=> Formula
             | Formula :<= Formula
             | Formula := Formula
             | Not Formula
             deriving (Eq,Ord)


--infixl 5 :&, :|
infixl 4 :<=
infixr 4 :=>
--infixl 3 :==
infix 2 :=

isAtom :: Formula -> Bool
isAtom (Var _) = True
isAtom FTrue = True
isAtom FFalse = True
isAtom (Not _) = True
isAtom _ = False

fsize :: Formula -> Int
fsize FTrue = 0
fsize FFalse = 0
fsize (Var _) = 1
fsize (FAnd   s) = (foldr max 0 (map fsize s)) + 1
fsize (FOr    s) = (foldr max 0 (map fsize s)) + 1
fsize (FEquiv s) = (foldr max 0 (map fsize s)) + 1
fsize (a :=> b) = max (fsize a) (fsize b) + 1
fsize (a :<= b) = max (fsize a) (fsize b) + 1
fsize (a := b) = max (fsize a) (fsize b) + 1
fsize (Not f) = fsize f + 1


vars :: Formula -> [String]
vars f =
    case f of
      FTrue -> []
      FFalse -> []
      Var x -> [x]
      FAnd   s -> concat $ map vars s
      FOr    s -> concat $ map vars s
      FEquiv s -> concat $ map vars s
      f1 :=> f2 -> vars f1 `union` vars f2
      f1 :<= f2 -> vars f1 `union` vars f2
      f1 := f2 -> vars f1 `union` vars f2
      Not f -> vars f

{-
-- Normalised formulae are equivalent modulo associativity of binary ops
normal :: Formula -> Formula
normal ((f1 :& f2) :& f3) = normal $ f1 :& (f2 :& f3)
normal ((f1 :| f2) :| f3) = normal $ f1 :| (f2 :| f3)
normal ((f1 :== f2) :== f3) = normal $ f1 :== (f2 :== f3)
normal (f1 :& f2) = normal f1 :& normal f2
normal (f1 :| f2) = normal f1 :| normal f2
normal (f1 :=> f2) = normal f1 :=> normal f2
normal (f1 :<= f2) = normal f1 :<= normal f2
normal (f1 :== f2) = normal f1 :== normal f2
normal (f1 := f2) = normal f1 := normal f2
normal (Not f) = Not (normal f)
normal f = f

-}





replace s d f = undefined


replace' :: Formula -> Formula -> Formula -> Formula
replace'  s d  f | f == s = d 
	         | otherwise = emap (replace'  s d) f
		  where	emap g (FAnd   fs) = FAnd $ map g fs
			emap g (FOr    fs) = FOr $ map g fs
			emap g (FEquiv fs) = FEquiv $ map g fs
			emap g (f1 := f2) = (g f1) := (g f2)
			emap g (f1 :=> f2) = (g f1) :=> (g f2)
			emap g (f1 :<= f2) = (g f1) :<= (g f2)
			emap g (Not f) = Not (g f)
			emap g f = f


{-
eqFormula :: Formula -> Formula -> Just Formula
eqFormula (FAnd fs1) (FAnd fs2) = 
eqFormula (FOr fs1) (FOr fs2) = 
eqFormula (FEquiv fs1) (FEquiv fs2) = 
eqFormula f1 f2 = if (f1 == f2) then Just f1 else Nothing
-}

p = Var "p"
q = Var "q"
f1 = FAnd [p,q,p] :=> p