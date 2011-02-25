{-# LANGUAGE GADTs #-}
module Commands where

import Laws
import Formula
import Subst

data Command = LoadLaws FilePath
             | Reload
             | ShowLaws
             | AddLaw Law
             | ProveAuto Formula
             | Prove Formula
             | Quit
             | Nop
             | Show String
               deriving (Show)

data CommandAssistant = List
                      | ShowDeriv
                      | Auto
     		      | Use Law
		      | Leave
		      | NopAssistant
		      | BT
		      | Qed
		      | Goal
		      	deriving (Show)