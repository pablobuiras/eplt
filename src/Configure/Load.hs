module Configure.Load where

import Formula
import Formula.Parser (law, ParserException(..))
import Text.ParserCombinators.Parsec
import System.IO
import Control.Monad
import Laws
import Control.Exception

loadLawsFrom :: FilePath -> IO LawBank
loadLawsFrom f = readFile f >>= fmap (genLawPriority f) . mapM plaw . lines

plaw :: String -> IO (Either Law Formula)
plaw l = either (throw . ParserException) return $ parse law "<load.laws>" l