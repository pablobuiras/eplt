{-# LANGUAGE DeriveDataTypeable #-}
module Formula.Parser where

import Formula
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import Exceptions
import Control.Exception
import Data.Typeable

-- Lexer
lexer = P.makeTokenParser
        (haskellStyle { P.reservedNames = ["true", "false"],
                        P.reservedOpNames = ["/\\", "\\/", "~", "=>", "<=", "==", "=", "->"] })
whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
parens = P.parens lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer


-- Parser
var = fmap Var identifier <?> "variable"
table = [[Prefix ((reservedOp "~" >> return Not) <?> "negation")],
         [op "/\\" (:&) AssocLeft, op "\\/" (:|) AssocLeft],
         [op "=>" (:=>) AssocRight, op "<=" (:<=) AssocLeft],
         [op "==" (:==) AssocLeft, op "=" (:=) AssocNone]]
        where op s f =
                  Infix ((reservedOp s >> return f) <?> "operator")

formula = buildExpressionParser table term
term = parens formula
       <|> var
       <|> (reserved "true" >> return FTrue)
       <|> (reserved "false" >> return FFalse)
       <?> "proposition"


lawPrim = do f <- formula
             reservedOp "->"
             f' <- formula
             return (f, f')

law = Text.ParserCombinators.Parsec.try (fmap Left lawPrim) <|> fmap Right formula

-- IO interface
data ParserException = ParserException ParseError
                     deriving Typeable

instance Show ParserException where
    show (ParserException pe) = show pe

instance Exception ParserException where
    toException = epltExceptionToException
    fromException = epltExceptionFromException

parser :: String -> String -> IO Formula
parser l = either (throwIO . ParserException) return . parse formula l
