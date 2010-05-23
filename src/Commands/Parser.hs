module Commands.Parser (command, parseCmd) where

import Commands
import Formula.Parser (law, formula, ParserException(..))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (haskellStyle)

import Exceptions
import Control.Exception
import Data.Typeable

-- Lexer
lexer = P.makeTokenParser
        (haskellStyle { P.reservedNames = [":loadLaws", ":addLaw", ":prove", ":quit", ":showLaws", ":reload"] })
whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
parens = P.parens lexer
identifier = P.identifier lexer
reserved = P.reserved lexer

-- Parser
command = choice [loadLaws,
                  showLaws,
                  addLaw,
                  prove,
                  quit,
                  reload,
                  fmap ProveAuto formula] >>= (eof >>) . return

reload = do reserved ":reload"
            return Reload

showLaws = do reserved ":showLaws"
              return ShowLaws

loadLaws = do reserved ":loadLaws"
              fp <- lexeme (many anyChar)
              return (LoadLaws fp)

addLaw = do reserved ":addLaw"
            l <- law
            return (AddLaw l)

prove = do reserved ":prove"
           f <- formula
           return (Prove f)

quit = reserved ":quit" >> return Quit

parseCmd :: String -> String -> IO Command
parseCmd l = either (throwIO . ParserException) return . parse command l
