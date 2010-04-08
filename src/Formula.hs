module Formula where

data TermFormula a = Var a
             | FTrue | FFalse
             | (TermFormula a) :& (TermFormula a)
             | (TermFormula a) :| (TermFormula a)
             | (TermFormula a) :=> (TermFormula a)
             | (TermFormula a) :<= (TermFormula a)
             | (TermFormula a) :== (TermFormula a)
             | (TermFormula a) := (TermFormula a)
             | Not (TermFormula a)
             deriving (Eq)

emap :: (TermFormula a -> TermFormula b) -> TermFormula a -> TermFormula b
emap g (f1 :& f2) = (g f1) :& (g f2)
emap g (f1 :| f2) = (g f1) :| (g f2)
emap g (f1 := f2) = (g f1) := (g f2)
emap g (f1 :=> f2) = (g f1) :=> (g f2)
emap g (f1 :<= f2) = (g f1) :<= (g f2)
emap g (f1 :== f2) = (g f1) :== (g f2)
emap g (Not f) = Not (g f)
emap g FTrue = FTrue
emap g FFalse = FFalse
emap g f = g f

instance Monad TermFormula where
	 return = Var
	 (Var x) >>= f = f x
	 z >>= f = emap (>>= f) z

type Formula = TermFormula String

infixl 5 :&, :|
infixl 4 :<=
infixr 4 :=>
infixl 3 :==
infix 2 :=

isAtom :: TermFormula a -> Bool
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
replace s d f | f == s = d
	      | otherwise = emap (replace s d) f
