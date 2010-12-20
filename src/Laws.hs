module Laws where

import Formula
import Formula.Pretty
import Subst


import Control.Monad
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

{-

testFormula = ( (Var "p" :| (Var "p" :& Var "q")) :== Var "p") 
testFormulb = ( (Var "p" :& (Var "p" :| Var "q")) :== Var "p") 
testFormulc = ( (Var "p" :| (Var "q" :| Var "r") ) :== ( (Var "p" :| Var "q") :| (Var "p" :| Var "r") ) )
testFormuld = ( (Var "p" :& Var "q") :== (Var "p" :| Var "q") :& Var "p" :& Var "q" ) 
testFormule = ( (Var "p" :| (Var "q" :& Var "r") ) :== ( (Var "p" :| Var "q") :& (Var "p" :| Var "r") ) )

-}

fore = (FAnd [Var "p", Var "q"]):=> (Var "r")

--testLaws = [ ((FAnd [Var "a", Var "b"], Var "z"),1 ),
--            --((Var "a" :=> Var "b", Not (Var "b") :=> Not (Var "a")),1 ),
--	     ( (Var "a" :=> Var "b", FOr [Not (Var "a"), Var "b"]), 1) ]

testBank = LB { laws = (map (\l -> (l,1)) testLaws), fileName = undefined }

testLaws = [  
	      
              -- Reflexividad
	      (FEquiv [Var "a",Var "a"], FTrue) , 

	      (FEquiv [FTrue, FFalse], FFalse),
	      (FEquiv [FTrue, FTrue], FTrue),

--	      (Not FTrue, FFalse),
--	      (Not FFalse, FTrue),
	      
	      -- Elementos neutros
	      (FEquiv [Var "a",FTrue], Var "a"),
	      (FAnd [Var "a", FTrue], Var "a"),
	      (FOr  [Var "a", FTrue], FTrue)
	      
	      -- Def =>
--	      (Var "a" :=> Var "b", Not (Var "b") :=> Not (Var "a") ),
	      
	      -- De morgan
--	      (Var "a" :=> Var "b", FOr [Not (Var "a"), Var "b"])
	      
	      -- Idempotencia
--	      (FAnd [Var "a", Var "a"], Var "a"),
--	      (FOr  [Var "a", Var "a"], Var "a")

	      
--	      (Var "a" :& FFalse, FFalse),

--	      (Var "a" :| Var "b", Not ((Not (Var "a")) :& (Not (Var "b")))),

              -- Asociatividad (faltarian agregar la inversas que a veces hacen falta)
--	      (Var "a" :== (Var "b" :== Var "c"), (Var "a" :== Var "b") :== Var "c"),
--	      (Var "a" :& (Var "b" :& Var "c"), (Var "a" :& Var "b") :& Var "c"),	      
--	      (Var "a" :| (Var "b" :| Var "c"), (Var "a" :| Var "b") :| Var "c"),
	      
	      -- Regla dorada
--	      ((Var "a" :& Var "b"), Var "a" :== (Var "b" :== (Var "a" :| Var "b"))), -- Beware!
--	      ((Var "a" :& Var "b") :==  Var "a", Var "b" :== (Var "a" :| Var "b")),
--	      ((Var "a" :& Var "b") :== (Var "a" :== Var "b"),(Var "a" :| Var "b")),  
--	      ((Var "a" :| Var "b"), Var "a" :== (Var "b" :== (Var "a" :& Var "b"))), -- Beware!
--	      ((Var "a" :| Var "b") :==  Var "a", Var "b" :== (Var "a" :& Var "b")),
--	      ((Var "a" :| Var "b") :== (Var "a" :== Var "b"),(Var "a" :& Var "b")), 
	      
	      -- Distributividad 
--	      (Var "a" :| (Var "b" :== Var "c"), (Var "a" :| Var "b") :== (Var "a" :| Var "c")),	      
--	      (Var "a" :| ( (Var "b" :== Var "c") :== (Var "a" :| Var "b") ), (Var "a" :| Var "c")),
	      
--	      (Var "a" :& (Var "b" :== Var "c"), ((Var "a" :& Var "b") :== (Var "a" :& Var "c")) :== Var "a"),
--	      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") ), (Var "a" :& Var "c") :== Var "a"),
--	      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") :== (Var "a" :& Var "c")) , Var "a"),

	      -- Conmutatividad
--	      (Var "a" :& Var "b", Var "b" :& Var "a") ,
--	      (Var "a" :| Var "b", Var "b" :| Var "a") ,
--	      (Var "a" :== Var "b", Var "b" :== Var "a")

              --(Var "a" :& Var "b", Var "b" :& Var "a"),

	   ]


unify :: (Functor m, MonadPlus m) => Subst -> m Subst
unify [] = return []
unify ((x,y):xs) | (ls == [] || ls == [y]) = fmap ((x,y):) (unify xs)
      		 | otherwise = mzero
    where ls = nub [b | (a,b) <- xs, a == x]


purify :: Formula -> Maybe Formula

purify (FAnd fs) = f fs'
                   where fs' = catMaybes $ map purify fs
		         f []   = Nothing
		         --f [x'] = Just x'
		         f  z   = Just $ FAnd z

purify (FOr fs) = f fs'
                   where fs' = catMaybes $ map purify fs
		         f []   = Nothing
		         --f [x'] = Just x'
		         f  z   = Just $ FOr z
			 
purify (FEquiv fs) = f fs'
                     where fs'    = catMaybes $ map purify fs
		           f []   = Nothing
		           f [x'] = Just x'
		           f  z   = Just $ FAnd z
			 
purify x = Just x

--fs1 = FAnd [Var "a", Var "b"]
--fs2 = FAnd [FTrue, Var "c", Var "a"]

parts n xs = filter ((==xs).concat) $ map (map (snd.unzip).reverse) $ filter ((== n) . length) $ partitionMultiset $ (zip [1..] xs)

match :: (MonadPlus m) => Formula -> Formula -> m Subst
match (Var p) f = return (p |-> f)
match FFalse FFalse = mzero
match FTrue FTrue =  mzero
match (Not f) (Not g) = match f g
match (FAnd fs1) (FAnd fs2) = gen FAnd fs1 fs2
match (FOr fs1) (FOr fs2) =  gen FOr fs1 fs2
match (FEquiv fs1) (FEquiv fs2) =  gen FEquiv fs1 fs2

match (f1 :=> f2) (f3 :=> f4) = msum [match f1 f3,match f2 f4]
match _ _ = return impsubst

gen op fs1 fs2 = msum       $ map return $ map msum
    	       	     	    $ map msum
			    -- $ filter (not. (any (== impsubst)))
			    $ map (zipWith match fs1)
                            $ map (\x -> catMaybes $ map purify $ map op x)	  
      	    	       	    $ concatMap (parts n)
			    $ permutations fs2
			      where n = length fs1

gen2 op fs1 fs2 =  id       $ map (zipWith match fs1)
                            $ map (\x -> catMaybes $ map purify $ map op x)	  
      	    	       	    $ concatMap (parts n)
			    $ permutations fs2
			      where n = length fs1



findLaws :: (Functor m, MonadPlus m) => Formula -> LawBank -> m (Law, Subst)
findLaws f (LB { laws = ls }) = foldr findLaw mzero ls
      where findLaw (l,_) m =  (matcher l f) `mplus` m 
            matcher l@(lhs,_) f =  fmap (\s -> (l,s)) $ (>>= unify) $ match lhs f
	    
applyl :: Formula -> (Law, Subst) -> Formula
applyl f ((lf, rf), s) = replace lf' rf' f
                         where lf' = substitute lf s
			       rf' = substitute rf s

substitute :: Formula -> Subst -> Formula
substitute f = foldr (\(s,d) -> replace (Var s) d ) f 

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

constrainLB :: Law -> LawBank -> Maybe LawBank
constrainLB l@(lhs,rhs) lb@(LB { laws = lsp }) =
    do let ls = map fst lsp
       _ <- foldr (\(a,b) m -> (match (a :== b) (lhs :== rhs) >>= unify) `mplus` m) mzero ls
       return (lb { laws = [(l,0)] })



genLawPriority :: FilePath -> [Either Law Formula] -> LawBank
genLawPriority fp lf = LB (sortBy (comparing snd) $ zip ls $ map ( \(l,r) -> ((fsize r) - (fsize l))) ls) fp
    where ls = concatMap (either (:[]) expand) lf

-}


genLawPriority :: FilePath -> [Either Law Formula] -> LawBank
genLawPriority fp lf = LB (map (\l -> (l,1)) (lefts lf)) fp

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


enumLaws :: (Functor m, MonadLogic m) => LawBank -> Formula -> m (Law, Subst)
enumLaws ls f = findLaws f ls
{-
	      	  Var x -> mzero
                  FTrue -> mzero
                  FFalse -> mzero
                  Not f -> findLaws f ls `mplus` enumLaws ls f
                  FAnd s -> findLaws f ls 
		            `mplus` 
			    (msum $ map (\f'-> findLaws f' ls) (expFormulas FAnd s)) 
			    `mplus` 
			    (msum $ map (enumLaws ls) s) 
                  FOr s -> findLaws f ls 
		            `mplus` 
		            (msum $ map (\f'-> findLaws f' ls) (expFormulas FOr s)) 
			    `mplus` 
			    (msum $ map (enumLaws ls) s) 
                  FEquiv s->findLaws f ls 
		            `mplus` 
		            (msum $ map (\f'-> findLaws f' ls) (expFormulas FEquiv s)) 
			    `mplus` 
			    (msum $ map (enumLaws ls) s) 
                   
		  
		  --FOr s -> findLaws f ls `mplus` (fold (\f m -> m `mplus` enumLaws ls f) mzero s)
                  --FEquiv s -> findLaws f ls `mplus` (fold (\f m -> m `mplus` enumLaws ls f) mzero s)
		  s :=> t -> findLaws f ls `mplus` (enumLaws ls s) `mplus` (enumLaws ls t)
-}

expFormulas op fs =  map op (powerset fs)  		  
powerset s = filterM (const [True, False]) s

