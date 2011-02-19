module Formula where


import Data.List
import Maybe
import Math.Combinat.Partitions

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

data CFormula = CNot
             | CFAnd   ([Formula],[Formula]) 
             | CFOr    ([Formula],[Formula])
	     | CFEquiv ([Formula],[Formula])
             | CFAndPart   ([Formula],[Formula]) 
             | CFOrPart    ([Formula],[Formula])
	     | CFEquivPart ([Formula],[Formula])
             | CImpRight Formula
             | CImpLeft  Formula
             deriving (Eq,Ord)

type ZFormula = ([CFormula],Formula)

mkZFormula :: Formula -> ZFormula
mkZFormula f = ([], f)

chooseNot :: ZFormula -> ZFormula
chooseNot (cs, Not f) = (CNot:cs, f)
chooseNot  _          = error "Error in chooseNot"

chooseAnd :: ZFormula ->  Int -> ZFormula
chooseAnd (cs, FAnd fs) i = ((CFAnd (fs1,fs2)):cs, f')
                            where (fs1,f':fs2) = splitAt (i) fs
chooseAnd  _            _ = error "Error in chooseAnd"

chooseOr :: ZFormula ->  Int -> ZFormula
chooseOr (cs, FOr fs) i = ((CFOr (fs1,fs2)):cs, f')
                            where (fs1,f':fs2) = splitAt (i) fs
chooseOr  _            _ = error "Error in chooseOr"   

chooseEquiv :: ZFormula ->  Int -> ZFormula
chooseEquiv (cs, FEquiv fs) i = ((CFEquiv (fs1,fs2)):cs, f')
                               where (fs1,f':fs2) = splitAt (i) fs
chooseEquiv  _            _ = error "Error in chooseEquiv"   

chooseImpLeft :: ZFormula -> ZFormula
chooseImpLeft (cs, f1 :=> f2) = (CImpLeft f1 : cs, f2)

chooseImpRight :: ZFormula -> ZFormula
chooseImpRight (cs, f1 :=> f2) = (CImpRight f2 : cs, f1)

chooseAndPart :: ZFormula -> Formula -> ZFormula
chooseAndPart (cs, FAnd fs) (FAnd pfs) = ((CFAndPart (pfs,diff)):cs, undefined)
                            where diff = fs \\ pfs
chooseAndPart  _            _ = error "Error in chooseAndPart"


chooseOrPart :: ZFormula -> Formula -> ZFormula
chooseOrPart (cs, FOr fs) (FOr pfs) = ((CFOrPart (pfs,diff)):cs, undefined)
                            where diff = fs \\ pfs
chooseOrPart  _            _ = error "Error in chooseOrPart"

chooseEquivPart :: ZFormula -> Formula -> ZFormula
chooseEquivPart (cs, FEquiv fs) (FEquiv pfs) = ((CFEquivPart (pfs,diff)):cs, undefined)
                            where diff = fs \\ pfs
chooseEquivPart  _            _ = error "Error in chooseEquivPart"


fill :: ZFormula -> Formula
fill ([], f)                  = f
fill (CNot : cs , f)          = fill (cs, Not f)
fill (CImpLeft f1 : cs , f2)  = fill (cs, f1 :=> f2)
fill (CImpRight f2 : cs , f1) = fill (cs, f1 :=> f2)
fill (CFAnd (fs1,fs2) : cs , f) = fill (cs, FAnd (fs1++[f]++fs2))
fill (CFOr (fs1,fs2) : cs , f) = fill (cs, FOr (fs1++[f]++fs2))
fill (CFEquiv (fs1,fs2) : cs , f) = fill (cs, FEquiv (fs1++[f]++fs2))


fill (CFAndPart (pfs,diff) : cs , _) = fill (cs, FAnd (pfs++diff))
fill (CFOrPart (pfs,diff) : cs , _) = fill (cs, FOr (pfs++diff))
fill (CFEquivPart (pfs,diff) : cs , _) = fill (cs, FEquiv (pfs++diff))
fill _                               = error "Malformed ZFormula"

getFormula :: ZFormula -> Formula
getFormula (CFAndPart (pfs,_):cs, _ ) = FAnd pfs
getFormula (CFOrPart (pfs,_):cs, _ ) = FOr pfs
getFormula (CFEquivPart (pfs,_):cs, _ ) = FEquiv pfs
getFormula (_ , f )  = f

infixl 4 :<=
infixr 4 :=>
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

fore = FOr [(Var "r"), (FAnd [Var "b", Var "a", FOr [Var "c", Var "d"] ])]
--fore2 = FOr [(FAnd [Var "a", Var "b", FOr [Var "d", Var "c"]]), (Var "r")]


replace :: Formula -> Formula -> Formula -> Formula
replace  s d  f | (expEqu f s) = d 
	        | otherwise = emap (replace  s d) f
		  where	emap g (FAnd   fs) = FAnd $ map g fs
			emap g (FOr    fs) = FOr $ map g fs
			emap g (FEquiv fs) = FEquiv $ map g fs
			emap g (f1 := f2) = (g f1) := (g f2)
			emap g (f1 :=> f2) = (g f1) :=> (g f2)
			emap g (f1 :<= f2) = (g f1) :<= (g f2)
			emap g (Not f) = Not (g f)
			emap g f = f

expEqu :: Formula -> Formula -> Bool
expEqu (Var p) (Var q) = p == q
expEqu FFalse FFalse = True
expEqu FTrue FTrue =  True
expEqu (Not f) (Not g) = expEqu f g
expEqu (FAnd fs1) (FAnd fs2) = compF FAnd fs1 fs2
expEqu (FOr fs1) (FOr fs2) =  compF FOr fs1 fs2
expEqu (FEquiv fs1) (FEquiv fs2) =  compF FEquiv fs1 fs2
expEqu (f1 :=> f2) (f3 :=> f4) =  (expEqu f1 f3) && (expEqu f2 f4)
expEqu _ _ = False

compF :: ([Formula] -> Formula) -> [Formula] -> [Formula] -> Bool
compF op fs1 fs2 | length(fs1) /= length(fs2) = False
                 | otherwise = b
               where b  =   or
                            $ map and
                            $ map (zipWith expEqu fs1)
                            $ map (\x -> catMaybes $ map purify $ map op x)	  
                            $ concatMap (parts n)
                            $ permutations fs2
                                  where n = length fs1
                                
  
purify :: Formula -> Maybe Formula

purify (FAnd fs) = f fs'
                   where fs' = catMaybes $ map purify fs
		         f []   = Nothing
		         f [x'] = Just x'
		         f  z   = Just $ FAnd z

purify (FOr fs) = f fs'
                   where fs' = catMaybes $ map purify fs
		         f []   = Nothing
		         f [x'] = Just x'
		         f  z   = Just $ FOr z
			 
purify (FEquiv fs) = f fs'
                     where fs'    = catMaybes $ map purify fs
		           f []   = Nothing
		           f [x'] = Just x'
		           f  z   = Just $ FAnd z
			 
purify x = Just x

parts n xs = filter ((==xs).concat) $ map (map (snd.unzip).reverse) $ filter ((== n) . length) $ partitionMultiset $ (zip [1..] xs)

