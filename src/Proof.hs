module Proof where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Monoid
import Laws
import Subst
import Data.List

data Proof = Proof { formulae :: [Formula], laws :: [Law], substs :: [Subst] }

instance Monoid Proof where
  mempty = Proof { formulae = mempty, laws = mempty, substs = mempty }
  mappend p p' = Proof { formulae = ((formulae p) `mappend` (formulae p')),
                         laws = ((laws p) `mappend` (laws p')),
                         substs = ((substs p) `mappend` (substs p'))
                         }

proofstep :: Formula -> Law -> Subst -> Proof
proofstep f l s = Proof { formulae = return f, laws = return l, substs = return s }


instance Show Proof where
    show (Proof { formulae = fs, laws = ls, substs = ss }) =
        (map showFormula fs `interleaveStr` map showLaw ls) ++ "true"
            where showFormula f = show f
                  showLaw (lhs,rhs) = "= { " ++ show lhs ++ " = " ++ show rhs ++ " }"
                  interleaveStr xs ys = concatMap (\(x,y) -> x ++ '\n':y ++ "\n") $ zip xs ys