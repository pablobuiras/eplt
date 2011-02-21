{-# LANGUAGE DeriveDataTypeable #-}
module ModelBuilder where

import Formula
import Control.Monad
import qualified Data.Map as M
import Exceptions
import Control.Exception
import Data.Typeable

-- for testing only
import Test.QuickCheck
import Data.List (nub)

data MBResult = Tauto | Counter Val
              deriving (Show, Eq)
type Val = M.Map String Bool

{-|
  Given a valuation, this function computes the boolean value of
  a formula, i.e. its semantics. 
-}
evalF :: Val -> Formula -> Maybe Bool
evalF v FTrue = return True
evalF v FFalse = return False
evalF v (Var x) = M.lookup x v
evalF v (FAnd fs) = mfoldOp (&&) True $ map (evalF v) fs
evalF v (FOr fs) = mfoldOp (||) False $ map (evalF v) fs
evalF v (f1 :=> f2) = evalF v (FOr [Not f1, f2])
evalF v (f1 :<= f2) = evalF v (FOr [Not f2, f1])
evalF v (FEquiv fs) = mfoldOp (==) True $ map (evalF v) fs
evalF v (f1 := f2) = liftM2 (==) (evalF v f1) (evalF v f2)
evalF v (Not f) = fmap not (evalF v f)

mfoldOp op z ls = fmap (foldr op z) $ sequence ls

{-|
  The function @enumCandidates@ enumerates all possible valuations for a set of variables (given as a list of strings).
-}
enumCandidates :: (Functor m, MonadPlus m) => [String] -> m Val
enumCandidates = foldr choose (return M.empty)
                 where choose v cs = fmap (M.insert v True) cs `mplus`
                                     fmap (M.insert v False) cs

-- The following properties should hold for enumCandidates
prop_enumCandidates1 vs =
    length (enumCandidates vs) == 2 ^ (length vs)
prop_enumCandidates2 vs =
    vs == nub vs ==> all (\val -> M.size val == length vs) (enumCandidates vs)

{-|
The function @tauto@ checks whether a given formula is a tautology. The result
of @tauto f@ is @'Tauto'@ if @f@ is a tautology, or @'Counter' v@ if it's not, where @v@ is a valuation that is not a model for @f@.
-}
tauto :: Formula -> MBResult
tauto f = foldr (checkModel f) Tauto $ enumCandidates $ vars f
    where checkModel f v r@(Counter _) = r
          checkModel f v Tauto = case evalF v f of
                                   Just True -> Tauto
                                   Just False -> Counter v
                                   _ -> error "assertion prop_enumCandidates failed"

{-|
The function @enumModels@ enumerates all models of a given formula.
-}
enumModels :: (Functor m, MonadPlus m) => Formula -> m Val
enumModels f = do val <- enumCandidates $ vars f
                  guard (isModel f val)
                  return val
    where isModel f = maybe False (==True) . flip evalF f


{-|
The function @sat@ checks whether a given formula is satisfiable. If it is,
the model is returned in the "Maybe" monad.
-}
sat :: Formula -> Maybe Val
sat = enumModels -- pona!

-- IO interface
data CounterException = CounterException Val
                      deriving Typeable

instance Show CounterException where
    show (CounterException v) = "Formula is not a tautology. Counterexample: " ++ concatMap show (M.toList v)

instance Exception CounterException where
    toException = epltExceptionToException
    fromException = epltExceptionFromException

modelCheck :: Formula -> IO ()
modelCheck f = case tauto f of
                 Counter v -> throwIO (CounterException v)
                 Tauto -> return ()