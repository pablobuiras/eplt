{-# LANGUAGE GADTs #-}
module Commands where

import Laws
import Formula

data Command = LoadLaws FilePath
             | ShowLaws
             | AddLaw Law
             | ProveAuto Formula
             | Prove Formula
             | Quit
             | Nop
               deriving (Show)

data CommandAssistant = List
     		      | Use Int
		      | Leave
		      | NopAssistant
		      | BT
		      | Qed
		      | Goal
		      	deriving (Show)