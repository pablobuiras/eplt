module Formula.Pretty where

import Formula
import Text.PrettyPrint.HughesPJ

data Context = And | Or | Impl | Conseq | Equiv | Top
             deriving (Eq)

contextOf :: Formula -> Context
contextOf (_ :& _) = And
contextOf (_ :| _) = Or
contextOf (_ :=> _) = Impl
contextOf (_ :<= _) = Conseq
contextOf (_ :== _) = Equiv
contextOf (_ := _) = Equiv
contextOf _ = Top


instance Ord Context where
    Top <= _ = False
    Equiv <= _ = True
    Impl <= Equiv = False
    Impl <= _ = True
    Conseq <= Equiv = False
    Conseq <= _ = True
    And <= Impl = False
    And <= Conseq = False
    And <= Equiv = False
    And <= _ = True
    Or <= Impl = False
    Or <= Conseq = False
    Or <= Equiv = False
    Or <= _ = True

binOp :: Doc -> Doc -> Doc -> Doc
binOp op d1 d2 = d1 <+> op <+> d2

pp :: Formula -> Doc
pp (Var v) = text v
pp FTrue = text "true"
pp FFalse = text "false"
pp (f1 :& f2) = binOp (text "/\\") (ppNest And f1) (ppNest And f2)
pp (f1 :| f2) = binOp (text "\\/") (ppNest Or f1) (ppNest Or f2)
pp (f1 :=> f2) = binOp (text "=>") (ppNest Top f1) (ppNest Impl f2)
pp (f1 :<= f2) = binOp (text "<=") (ppNest Conseq f1) (ppNest Top f2)
pp (f1 :== f2) = binOp (text "==") (ppNest Equiv f1) (ppNest Equiv f2)
pp (f1 := f2) = binOp (text "=") (ppNest Equiv f1) (ppNest Equiv f2)
pp (Not f) = char '~' <> parens (ppNest Top f)

ppNest :: Context -> Formula -> Doc
ppNest c f | not (isAtom f) && contextOf f <= c = parens (pp f)
           | otherwise = pp f

instance Show Formula where
    show = render . pp