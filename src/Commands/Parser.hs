module Commands.Parser (command, parseCmd, assistant, parseAssistant) where

import Commands
import Formula.Parser (lawPrim, formula, ParserException(..))
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (haskellStyle)

import Exceptions
import Control.Exception
import Data.Typeable

-- Lexers
-- Main lexer
lexer = P.makeTokenParser
        (haskellStyle { P.reservedNames = [":loadLaws", ":addLaw", ":prove", ":quit", ":showLaws", ":reload"] })

whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
parens = P.parens lexer
identifier = P.identifier lexer
reserved = P.reserved lexer

-- Proof assistant lexer
lexerPA = P.makeTokenParser
        (haskellStyle { P.reservedNames = ["use", "list", "leave","bt", "qed", "auto", "show"] })
whiteSpacePA = P.whiteSpace lexerPA
lexemePA = P.lexeme lexerPA
symbolPA = P.symbol lexerPA
parensPA = P.parens lexerPA
identifierPA = P.identifier lexerPA
reservedPA = P.reserved lexerPA
naturalPA = P.natural lexerPA


-- Parsers
-- Main parser
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
            l <- lawPrim
            return (AddLaw l)

prove = do reserved ":prove"
           f <- formula
           return (Prove f)

quit = reserved ":quit" >> return Quit

parseCmd :: String -> String -> IO Command
parseCmd l = either (throwIO . ParserException) return . parse command l

-- Proof assistant parser
assistant = choice [use, list, leave, bt, qed, goal, auto, showDeriv] >>= (eof >>) . return

showDeriv = do reservedPA "show"
               return ShowDeriv

auto = do reservedPA "auto"
          return Auto

use = do reservedPA "use"
      	 l <- lawPrim
	 return (Use l)

list = reservedPA "list" >> return List

leave = reservedPA "leave" >> return Leave

bt = reservedPA "bt" >> return BT

qed = reservedPA "qed" >> return Qed

goal = reservedPA "goal" >> return Goal

parseAssistant :: String -> String -> IO CommandAssistant
parseAssistant l = either (throwIO . ParserException) return . parse assistant l