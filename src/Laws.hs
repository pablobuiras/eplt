module Laws where

import Formula
import Formula.Pretty
import Subst

import Control.Monad
import Control.Monad.Logic
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Ord

type Law = (Formula, Formula)
data LawBank = LB { laws :: [(Law, Int)], fileName :: FilePath }

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
findLaws f (LB { laws = ls }) = foldr findLaw mzero ls
    where findLaw (l,_) m = matcher l f `mplus` m
          matcher l@(lhs,_) f = fmap (\s -> (l,s)) (match lhs f >>= unify)


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
genLawPriority fp lf = LB (sortBy (comparing snd) $ zip ls $ map ( \ (l,r) -> ((size r) - (size l))) ls) fp
    where ls = concatMap (either (:[]) expand) lf

showLawPriority :: LawBank -> IO ()
showLawPriority (LB { laws = lb }) = do  putStr $ (pr "Law") ++ "\t Priority\n"
                                         mapM_ ( \ (l,p) -> putStr (pr (showLaw l) ++ "\t   "++ show p ++ "\n")) lb


-- TODO: make show instances of Law and Subts
showLaw (l,r) = show l ++ " <=> "++show r
showSubts ss = concat $ map ( \ (v,e) -> v++" => "++show e++" , ") ss 

pr s = s ++ take w (repeat ' ')
        where w = 40 - length s

ordGenericLaws = snd

lawToFormula (lhs,rhs) = lhs :== rhs

formulaToLaw f@(lhs :== rhs) = Just f
formulaToLaw _ = Nothing

addLaw :: Law -> LawBank -> LawBank
addLaw l lb@(LB { laws = ls }) = genLawPriority (fileName lb) (Left l : map (Left . fst) ls)

enumLaws :: (Functor m, MonadLogic m) => LawBank -> Formula -> m (Law, Subst)
enumLaws ls f = case f of
                  Var x -> mzero
                  FTrue -> mzero
                  FFalse -> mzero
                  Not f -> findLaws f ls `mplus` enumLaws ls f
                  f1 :& f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2
                  f1 :| f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2
                  f1 :== f2 -> findLaws f ls `mplus` enumLaws ls f1 `mplus` enumLaws ls f2