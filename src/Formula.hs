module Formula where

data Formula = Var String
             | FTrue | FFalse
             | Formula :& Formula
             | Formula :| Formula
             | Formula :=> Formula
             | Formula :<= Formula
             | Formula :== Formula
             | Formula := Formula
             | Not Formula
             deriving (Show, Eq, Ord)

infixl 5 :&, :|
infixl 4 :<=
infixr 4 :=>
infixl 3 :==
infix 2 :=