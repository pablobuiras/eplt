{-# LANGUAGE TypeSynonymInstances #-}

module Test where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Text.ParserCombinators.Parsec
import Text.PrettyPrint.HughesPJ
import Formula
import Formula.Parser
import Formula.Pretty
import Control.Monad

instance Arbitrary Formula where
    arbitrary = frequency [(3,return FTrue),
                           (3,return FFalse),
                           (10,fmap Var (elements (map (:[]) "pqrstabcde"))),
                           (2,op (:&)), (2,op (:|)),
                           (1,op (:=>)), (1,op (:<=)),
                           (1,op (:==)),
                           (2,fmap Not arbitrary)]
                where op f = liftM2 f arbitrary arbitrary

instance CoArbitrary Formula where
    coarbitrary FTrue = variant 0 . coarbitrary "t"
    coarbitrary FFalse = variant 1 . coarbitrary "f"
    coarbitrary (Var s) = variant 2 . coarbitrary s
    coarbitrary (f1 :& f2) = variant 3 . coarbitrary f1 . coarbitrary f2
    coarbitrary (f1 :| f2) = variant 4 . coarbitrary f1 . coarbitrary f2
    coarbitrary (f1 :=> f2) = variant 5 . coarbitrary f1 . coarbitrary f2
    coarbitrary (f1 :<= f2) = variant 6 . coarbitrary f1 . coarbitrary f2
    coarbitrary (f1 :== f2) = variant 7 . coarbitrary f1 . coarbitrary f2
    coarbitrary (f1 := f2) = variant 8 . coarbitrary f1 . coarbitrary f2
    coarbitrary (Not f) = variant 9 . coarbitrary f

-- Pretty-printing and parsing must be inverses
prop_ParsePP f = case parse formula "<testing>" (render $ pp f) of
                   Left _ -> False
                   Right t -> normal t == normal f
