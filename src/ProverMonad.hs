{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module ProverMonad where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Error
import Formula
import Deriv
import Laws
import Subst

type Heuristics = (ProverState -> Formula -> LawBank -> LawBank,
                   ProverState -> Formula -> [(Law,Subst, ZFormula)] -> [(Law,Subst, ZFormula)],
                   ProverState -> Formula -> [Deriv] -> [Deriv])

-- magic number
expThreshold = 6000

data ProverState = PS { expanded :: Int, depth :: Int, visited :: [Formula] }
data ProverEnv = PE { lawBank :: LawBank, heuristics :: Heuristics }

instance Error Deriv where
  noMsg = undefined
  strMsg s = undefined

instance Show ProverState where
    show (PS { expanded = e, depth = n, visited = vs }) = "Search depth: " ++ show n ++ ", expanded nodes: " ++ show e ++ ", visited nodes: " ++ show (length vs)

newtype Prover d a = P { unP :: ReaderT ProverEnv
                                (LogicT
                                 (ErrorT d
                                 (State ProverState))) a }
	      deriving (Functor,
	                Monad,
	                MonadPlus,
			MonadLogic,
                        MonadError d)

runProver :: Error d => ProverState -> ProverEnv -> Prover d a -> (Either d [a],ProverState)
runProver initState pe = flip runState initState .
                         runErrorT .
                         observeAllT .
			 (flip runReaderT pe) .
			 unP

class (MonadLogic m) => MonadProver m d | m -> d where
   incNodes :: m ()
   incDepth :: m ()
   prune :: d -> m d
   constrainLaws :: d -> m a -> m a
   applyDerivH :: d -> m d -> m d
   applyLawH :: d -> m (Law,Subst, ZFormula) -> m (Law,Subst, ZFormula)
   applyAll :: ([a] -> [a]) -> m a -> m a
   getLawBank :: m LawBank
   
instance MonadProver (Prover Deriv) Deriv where
  incNodes = P $ modify (\r -> r { expanded = expanded r + 1 })
  incDepth = P $ modify (\r -> r { depth = depth r + 1 })
  prune d = P $ (fmap visited get >>= \vs ->
                   do guard (not (goal d `elem` vs))
                      exps <- fmap expanded get
                      when (exps > expThreshold) (throwError d)
                      modify (\r -> r { visited = goal d : visited r})
  	              return d)
  getLawBank = P $ asks lawBank
  applyAll f m = P $ do pe <- ask
                        st <- get
                        let (as, s') = runProver st pe m
                        case as of
                          Left e -> throwError e
                          Right as -> do put s'
                                         msum $ map return (f as)
  constrainLaws d (P m) = P $ do (h1,_,_) <- asks heuristics
                                 st <- get
                                 local (\r -> r { lawBank = h1 st (goal d) (lawBank r) }) m
  applyLawH d m = P $ do (_,h2,_) <- asks heuristics
                         st <- get
                         unP $ applyAll (h2 st (goal d)) m
  applyDerivH d m = P $ do (_,_,h3) <- asks heuristics
                           st <- get
                           unP $ applyAll (h3 st (goal d)) m