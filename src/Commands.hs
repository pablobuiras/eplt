{-# LANGUAGE GADTs #-}
module Commands where

import Laws
import Formula

data Command = LoadLaws FilePath
             | AddLaw Law
             | ProveAuto Formula
             | Prove Formula
             | Quit
               deriving (Show)

