module Formula.Parser where

import Formula
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (haskellStyle)

-- Lexer
lexer = P.makeTokenParser
        (haskellStyle { P.reservedNames = ["true", "false"],
                        P.reservedOpNames = ["/\\", "\\/", "~", "=>", "<=", "==", "="] })
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