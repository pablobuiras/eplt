import Formula
import Formula.Pretty
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List
import Control.Monad.Logic.Class
import Debug.Trace


testLaws :: [Law]
testLaws = [  
	      
	      -- Conmutatividad
	      (Var "a" :== Var "b", Var "b" :== Var "a") ,
	      (Var "a" :& Var "b", Var "b" :& Var "a") ,
	      (Var "a" :| Var "b", Var "b" :| Var "a") ,

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
	      ((Var "a" :& Var "b"), Var "a" :== (Var "b" :== (Var "a" :| Var "b"))),
	      ((Var "a" :& Var "b") :==  Var "a", Var "b" :== (Var "a" :| Var "b")),
	      ((Var "a" :& Var "b") :== (Var "a" :== Var "b"),(Var "a" :| Var "b")),
	      ((Var "a" :| Var "b"), Var "a" :== (Var "b" :== (Var "a" :& Var "b"))),
	      ((Var "a" :| Var "b") :==  Var "a", Var "b" :== (Var "a" :& Var "b")),
	      ((Var "a" :| Var "b") :== (Var "a" :== Var "b"),(Var "a" :& Var "b")),
	      
	      -- Distributividad 
	      (Var "a" :| (Var "b" :== Var "c"), (Var "a" :| Var "b") :== (Var "a" :| Var "c")),	      
	      (Var "a" :| ( (Var "b" :== Var "c") :== (Var "a" :| Var "b") ), (Var "a" :| Var "c")),
	      
	      (Var "a" :& (Var "b" :== Var "c"), ((Var "a" :& Var "b") :== (Var "a" :& Var "c")) :== Var "p"),	      
	      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") ), (Var "a" :& Var "c") :== Var "p"),
	      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") :== (Var "a" :& Var "c")) , Var "p")


              --(Var "a" :& Var "b", Var "b" :& Var "a"),

	   ]
ordLaws x = case x of 
                      -- Reglas que SIEMPRE conviene seleccionar primero (reducen estructura)
                      (Var "a" :== Var "a", FTrue) 	     	    -> 0
		      (FTrue :== FFalse, FFalse)		    -> 0
		      (FTrue :== FTrue, FTrue)		    	    -> 0
		      (Not FTrue, FFalse)			    -> 0
		      (Not FFalse, FTrue)			    -> 0
		      (Var "a" :== FTrue, Var "a")		    -> 0
		      (Var "a" :& FTrue , Var "a")		    -> 0
		      (Var "a" :& Var "a", Var "a")		    -> 0
		      (Var "a" :| Var "a", Var "a")		    -> 0
		      (Var "a" :| FTrue, FTrue)		    	    -> 0
		      (Var "a" :& FFalse, FFalse)		    -> 0
		      
		      -- Reglas que conviene aplicar en 2do lugar
		      -- Golden rule
		      ((Var "a" :& Var "b"), Var "a" :== (Var "b" :== (Var "a" :| Var "b"))) -> 1
		      ((Var "a" :& Var "b") :==  Var "a", Var "b" :== (Var "a" :| Var "b")) -> 1
		      ((Var "a" :& Var "b") :== (Var "a" :== Var "b"),(Var "a" :| Var "b")) -> 1
		      ((Var "a" :| Var "b"), Var "a" :== (Var "b" :== (Var "a" :& Var "b"))) -> 1
		      ((Var "a" :| Var "b") :==  Var "a", Var "b" :== (Var "a" :& Var "b")) -> 1
		      ((Var "a" :| Var "b") :== (Var "a" :== Var "b"),(Var "a" :& Var "b")) -> 1
		      
		      
		      -- Reglas que conviene aplicar en ultimo lugar
		      
		      -- Permutatividad
                      (Var "a" :== Var "b", Var "b" :== Var "a") -> 2
		      (Var "a" :& Var "b", Var "b" :& Var "a")   -> 2
		      (Var "a" :| Var "b", Var "b" :| Var "a")   -> 2
		      
		      --(Var "a" :| Var "b", Not ((Not (Var "a")) :& (Not (Var "b")))) -> 2
		      
		      -- Asociatividad
		      (Var "a" :== (Var "b" :== Var "c"), (Var "a" :== Var "b") :== Var "c") -> 2
		      (Var "a" :& (Var "b" :& Var "c"), (Var "a" :& Var "b") :& Var "c") -> 2
		      (Var "a" :| (Var "b" :| Var "c"), (Var "a" :| Var "b") :| Var "c") -> 2
		      
		      -- Distributidad
		      (Var "a" :| (Var "b" :== Var "c"), (Var "a" :| Var "b") :== (Var "a" :| Var "c")) ->  1	      
		      (Var "a" :| ( (Var "b" :== Var "c") :== (Var "a" :| Var "b") ) , (Var "a" :| Var "c")) -> 0
		      
		      
		      (Var "a" :& (Var "b" :== Var "c"), ((Var "a" :& Var "b") :== (Var "a" :& Var "c")) :== Var "p") -> 1  
		      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") ), (Var "a" :& Var "c") :== Var "p")-> 0  
		      (Var "a" :& ( (Var "b" :== Var "c") :== (Var "a" :& Var "b") :== (Var "a" :& Var "c") ) , Var "p")-> 0
		      

