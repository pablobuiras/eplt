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

data AssistantCommand = ListLaws
     		      | ApplyLaw (Law, Subst)
		      | QuitAssistant
		      	deriving (Show)