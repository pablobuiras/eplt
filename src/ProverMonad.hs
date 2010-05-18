{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module ProverMonad where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Logic
import Control.Monad.State
import Formula
import Deriv
import Laws

data ProverState = PS { expanded :: Int, depth :: Int, visited :: [Formula] }

instance Show ProverState where
    show (PS { expanded = e, depth = n, visited = vs }) = "Search depth: " ++ show n ++ ", expanded nodes: " ++ show e ++ ", visited nodes: " ++ show (length vs)

newtype Prover a = P { unP :: ReaderT LawBank
			      (LogicT
			      (State ProverState)) a }
	      deriving (Functor,
	                Monad,
	                MonadPlus,
			MonadLogic)

runProver :: ProverState -> LawBank -> Prover a -> ([a],ProverState)
runProver initState lb = flip runState initState .
                         observeAllT .
			 (flip runReaderT lb) .
			 unP

class MonadLogic m => MonadProver m a | m -> a where
   incNodes :: m ()
   incDepth :: m ()
   prune :: a -> m a
   applyAll :: ([a] -> [a]) -> m a -> m a
   getLawBank :: m LawBank
   
instance MonadProver Prover Deriv where
  incNodes = P $ modify (\r -> r { expanded = expanded r + 1 })
  incDepth = P $ modify (\r -> r { depth = depth r + 1 })
  prune d = P $ (fmap visited get >>= \vs ->
                   do guard (not (goal d `elem` vs))
                      modify (\r -> r { visited = goal d : visited r})
  	              return d)
  getLawBank = P $ ask
  applyAll f m = undefined