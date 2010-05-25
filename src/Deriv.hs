module Deriv where

import Formula
import Formula.Pretty
import Control.Monad
import Data.Monoid
import Laws
import Subst
import Data.List

data Step = Step { law :: Law, subst :: Subst }
data Deriv = Deriv { formulae :: [Formula], steps :: [Step] }

startDeriv :: Formula -> Deriv
startDeriv f = Deriv { formulae = [f], steps = [] }

derivStep :: Deriv -> Formula -> (Law,Subst) -> Deriv
derivStep d f (l,s) = let f' = applyl f (l,s)
                      in d { formulae = f' : formulae d,
                             steps = Step l s : steps d }

tailDeriv :: Deriv -> Deriv
tailDeriv (Deriv fs ss) = Deriv (init fs) ss

appendDeriv :: Deriv -> Deriv -> Deriv
appendDeriv (Deriv fs1 ss1) (Deriv fs2 ss2) = Deriv (fs2 ++ fs1) (ss2 ++ ss1)

goal :: Deriv -> Formula
goal (Deriv { formulae = f : _ }) = f

qed :: Deriv -> Bool
qed d = goal d == FTrue

derivUnstep :: Deriv -> Deriv
derivUnstep (Deriv { formulae = f : fs, steps = s : ss }) = Deriv { formulae = fs, steps = ss}

instance Show Step where
    show (Step { law = (lhs,rhs), subst = s}) = "= { " ++ show lhs ++ " = " ++ show rhs ++ " }"

instance Show Deriv where
    show (Deriv { formulae = fs, steps = ss }) =
        let g : xs = reverse fs
        in show g ++ '\n':(map show (reverse ss) `interleaveStr` map showFormula xs)
            where showFormula f = show f
                  interleaveStr xs ys = concatMap (\(x,y) -> x ++ '\n':y ++ "\n") $ zip xs ys