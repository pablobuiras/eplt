module Subst where

import Formula

type Subst = [(String, Formula)]
(|->) :: String -> Formula -> Subst
v |-> f = [(v,f)]

impsubst = "" |-> undefined

