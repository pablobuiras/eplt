module Formula where

data Formula = Var String
             | FTrue | FFalse
             | Formula :& Formula
             | Formula :| Formula
             | Formula :=> Formula
             | Formula :<= Formula
             | Formula :== Formula
             | Formula := Formula
             | Not Formula
             deriving (Eq)

infixl 5 :&, :|
infixl 4 :<=
infixr 4 :=>
infixl 3 :==
infix 2 :=

isAtom :: Formula -> Bool
isAtom (Var _) = True
isAtom FTrue = True
isAtom FFalse = True
isAtom (Not _) = True
isAtom _ = False


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

replace :: Formula -> Formula -> Formula -> Formula
replace s d f | normal f == normal s = d
	      | otherwise = emap (replace s d) f
		where	emap g (f1 :& f2) = (g f1) :& (g f2)
			emap g (f1 :| f2) = (g f1) :| (g f2)
			emap g (f1 := f2) = (g f1) := (g f2)
			emap g (f1 :=> f2) = (g f1) :=> (g f2)
			emap g (f1 :<= f2) = (g f1) :<= (g f2)
			emap g (f1 :== f2) = (g f1) :== (g f2)
			emap g (Not f) = Not (g f)
			emap g f = f

