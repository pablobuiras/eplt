module Laws where

import Formula
import Formula.Pretty
import Subst


import Control.Monad.Logic
import Data.Monoid
import Data.Maybe
import Data.List hiding (union)
import Data.Ord
import Data.Either

import Math.Combinat.Partitions

type Law = (Formula, Formula)
data LawBank = LB { laws :: [(Law, Int)], fileName :: FilePath }

type SComp = (Law,Subst)
type SComps = [SComp]

testFormula = FEquiv [FOr [Var "p" , FAnd [Var "p" , Var "q"]], Var "p"]
testFormulb = FEquiv [FAnd [Var "p" , FOr [Var "p" , Var "q"]], Var "p"]
testFormulc = FEquiv [ FOr [Var "p" , FOr [Var "q" , Var "r"]] , FOr [FOr [Var "p" , Var "q"] , Var "p" ,Var "r"]]
testFormuld = FEquiv [ FAnd [Var "p" , Var "q"] , FAnd[ FOr [Var "p" , Var "q"] , Var "p" , Var "q"] ]
testFormule = FEquiv [ FOr [Var "p" , FAnd [Var "q" , Var "r"] ] , FAnd [ FOr [Var "p" , Var "q"] , FOr [Var "p" , Var "r"] ] ]
testFormulf = FEquiv [ FAnd [Var "p" , FOr [Var "q" , Var "r"] ] , FOr [ FAnd [Var "p" , Var "q"] , FAnd [Var "p" , Var "r"] ] ]

testBank = LB { laws = sortBy (comparing snd) (map (\l -> (l,lawPriority l)) testLaws), fileName = undefined }

testLaws = [
            -- Reflexividad
            (FEquiv [Var "a",Var "a"], FTrue) ,
	    (FEquiv [FTrue, FFalse], FFalse),
	    (FEquiv [FTrue, FTrue], FTrue),
            -- Idempotencia
	    (FAnd [Var "a", Var "a"], Var "a"),
	    (FOr  [Var "a", Var "a"], Var "a"),
            -- Elementos neutros
            (FEquiv [Var "a",FTrue], Var "a"),
	    (FAnd [Var "a", FTrue], Var "a"),
            
            -- Elemento absorvente
            (FOr [Var "a",FTrue], FTrue),
            -- Contrareciproco
	    (Var "a" :=> Var "b", Not (Var "b") :=> Not (Var "a")),
            
            -- Distributividad de ==
            (FEquiv [FOr [Var "a" , Var "b"], FOr [Var "a" , Var "c"]],FOr [Var "a",FEquiv [Var "b" , Var "c"]]),
            --(FEquiv [FOr [Var "a" , Var "b"], FOr [Var "a" , Var "c"]],FOr [Var "a",FEquiv [Var "b" , Var "c"]])
            
            
            -- Regla Dorada
            (FEquiv [FAnd [Var "a" , Var "b"],Var "a", Var "b" ], FOr [Var "a" , Var "b"]),
            (FEquiv [FAnd [Var "a" , Var "b"],Var "a"], FEquiv [Var "b" , FOr [Var "a" , Var "b"]]),
            
            
            (FEquiv [FOr [Var "a" , Var "b"], Var "a"],FEquiv [Var "b",FAnd [Var "a" , Var "b"]]),
            (FEquiv [FOr [Var "a" , Var "b"], Var "a", Var "b"],FAnd [Var "a" , Var "b"]),
            
            (FAnd [Var "a" , Var "b"], FEquiv [Var "a" , Var "b" , FOr [Var "a" , Var "b"]]),
            (FOr  [Var "a" , Var "b"], FEquiv [Var "a" , Var "b" , FAnd [Var "a" , Var "b"]])

	   ]


unify :: (Functor m, MonadPlus m) => Subst -> m Subst
unify [] = return []
unify ((x,y):xs) | (ls == [] || ls == [y]) = fmap ((x,y):) (unify xs)
      		 | otherwise = mzero
    where ls = nub [b | (a,b) <- xs, a == x]


match :: (Functor m, MonadPlus m) => Formula -> Formula -> m Subst
match (Var p) f = return (p |-> f)
match FFalse FFalse = return []
match FTrue FTrue =  return []
match (Not f) (Not g) = match f g
match (FAnd fs1) (FAnd fs2) = gen FAnd fs1 fs2
match (FOr fs1) (FOr fs2) =  gen FOr fs1 fs2
match (FEquiv fs1) (FEquiv fs2) =  gen FEquiv fs1 fs2

match (f1 :=> f2) (f3 :=> f4) =  merge (match f1 f3) (match f2 f4)
match _ _ = mzero

gen :: (Functor m, MonadPlus m) => ([Formula] -> Formula) -> [Formula] -> [Formula] -> m Subst
gen op fs1 fs2 =               msum
                                $ map prod
                                $ map (zipWith match fs1)
                                $ map (\x -> catMaybes $ map purify $ map op x)	  
                                $ concatMap (parts n)
                                $ permutations fs2
                                  where n = length fs1


mergeSubstSubst :: Subst -> Subst -> Subst
mergeSubstSubst = (++)

mergeSubstTomSubst :: (Functor m, MonadPlus m) =>  m Subst -> Subst -> m Subst
mergeSubstTomSubst ms s =  ms >>= (return . flip mergeSubstSubst s)

merge :: (Functor m, MonadPlus m) => m Subst -> m Subst -> m Subst
merge ms1 ms2 =  join $ fmap (mergeSubstTomSubst ms1) ms2

prod :: (Functor m,MonadPlus m) => [m Subst] -> m Subst
prod [] = mzero
prod xs = foldr1 merge xs

findLaws :: (Functor m, MonadPlus m) => ZFormula -> LawBank -> m (Law, Subst, ZFormula)
findLaws zf (LB { laws = ls }) = foldr findLaw mzero ls
      where f = getFormula zf 
            findLaw (l,_) m =  (matcher l f) `mplus` m 
            matcher l@(lhs,_) f =  fmap (\s -> (l,s, zf)) $ (>>= unify) $ match lhs f
	    
applyl :: Formula -> (Law, Subst, ZFormula) -> Formula
applyl _ ((lf, rf), s, zf) = normalize $ reconstruct zf (replace lf' rf' f)
                         where lf' = substitute lf s
			       rf' = substitute rf s
                               f   = getFormula zf

substitute :: Formula -> Subst -> Formula
substitute f s = normalize $ foldr (\(s,d) -> replace (Var s) d ) f s

mapSnd :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnd f = map (\(x,y) -> (x, f y))

mapFst :: (a -> b) -> [(a,c)] -> [(b,c)]
mapFst f = map (\(x,y) -> (f x, y))

{-

expand :: Formula -> [Law]
expand f@(_ :== _) =  let lvl = expandLvl f
                      in nub $ lvl ++ concatMap (\(lhs,rhs) -> let ls = expand rhs
                                                               in mapFst (lhs :==) ls ++ mapSnd (lhs :==) ls) lvl
expand f = []

expandLvl :: Formula -> [Law]
expandLvl = mapSnd (foldr1 (:==)) . pivots . breakEqs

pivots [] = []
pivots (f:fs) = (f,fs) : mapSnd (f:) (pivots fs)

breakEqs f = case f of
                         lhs :== rhs -> breakEqs lhs ++ breakEqs rhs
                         _ -> [f]
-}

constrainLB :: Law -> LawBank -> Maybe LawBank
constrainLB l@(lhs,rhs) lb@(LB { laws = lsp }) =
    do let ls = map fst lsp
       _ <- foldr (\(a,b) m -> (match (FEquiv [a, b]) (FEquiv [lhs, rhs]) >>= unify) `mplus` m) mzero ls
       return (lb { laws = [(l,0)] })


{-
genLawPriority :: FilePath -> [Either Law Formula] -> LawBank
genLawPriority fp lf = LB (sortBy (comparing snd) $ zip ls $ map ( \(l,r) -> ((fsize r) - (fsize l))) ls) fp
    where ls = concatMap (either (:[]) expand) lf

-}
lawPriority :: Law -> Int
lawPriority (l,r) = (fsize r) - (fsize l)

genLawPriority :: FilePath -> [Either Law Formula] -> LawBank
genLawPriority fp lf = LB (map (\l -> (l,lawPriority l)) (lefts lf)) fp

showLawPriority :: LawBank -> IO ()
showLawPriority (LB { laws = lb }) = do  putStr $ (pr "Law") ++ "\t Priority\n"
                                         mapM_ ( \ (l,p) -> putStr (pr (showLaw l) ++ "\t   "++ show p ++ "\n")) lb


-- TODO: make show instances of Law and Subts
showLaw (l,r) = show l ++ " <=> "++show r
showSubts ss = concat $ map ( \ (v,e) -> v++" => "++show e++" , ") ss 

pr s = s ++ take w (repeat ' ')
        where w = 40 - length s

ordGenericLaws = snd

{-
lawToFormula (lhs,rhs) = lhs :== rhs

formulaToLaw f@(lhs :== rhs) = Just f
formulaToLaw _ = Nothing
-}

addLaw :: Law -> LawBank -> LawBank
addLaw l lb@(LB { laws = ls }) = genLawPriority (fileName lb) (Left l : map (Left . fst) ls)

enumLaws :: (Functor m , MonadLogic m) => LawBank -> ZFormula -> m (Law, Subst, ZFormula)
enumLaws ls zf =  findLaws zf ls
                  `mplus`
                  case getFormula zf of
                  Not f -> findLaws zf' ls `mplus` enumLaws ls zf'
                            where zf' = (chooseNot zf)
                  FAnd s -> msum (map (\f -> findLaws (chooseAndPart zf f) ls) (expFormulas FAnd s))
                            `mplus` msum (map (enumLaws ls . (chooseAnd zf)) [0.. (length s)-1])
                  FOr s -> msum (map (\f -> findLaws (chooseOrPart zf f) ls) (expFormulas FOr s))
                            `mplus` msum (map (enumLaws ls . (chooseOr zf)) [0.. (length s)-1])
                  FEquiv s -> msum (map (\f -> findLaws (chooseEquivPart zf f) ls) (expFormulas FEquiv s))
                            `mplus` msum (map (enumLaws ls . (chooseEquiv zf)) [0.. (length s)-1])
                  f1 :=> f2 -> (findLaws zf1' ls) `mplus` (findLaws zf2' ls) `mplus`  
                                enumLaws ls zf1' `mplus` enumLaws ls zf2'
                            where zf1' = (chooseImpLeft zf)
                                  zf2' = (chooseImpRight zf)
                  _     -> mzero
