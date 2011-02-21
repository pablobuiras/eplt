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
import Control.Arrow

data TF = TVar String | TFalse | TTrue
        | TAnd TF TF | TOr TF TF | TEq TF TF
        | TImpl TF TF | TConseq TF TF | TEqual TF TF
        | TNot TF
          deriving (Eq, Show)

toFormula :: TF -> Formula
toFormula f = normalize $ case f of
  TVar s -> Var s
  TFalse -> FFalse
  TTrue -> FTrue
  TAnd f1 f2 -> FAnd [toFormula f1, toFormula f2]
  TOr f1 f2 -> FOr [toFormula f1, toFormula f2]
  TEq f1 f2 -> FEquiv [toFormula f1, toFormula f2]
  TImpl f1 f2 -> toFormula f1 :=> toFormula f2
  TConseq f1 f2 -> toFormula f1 :<= toFormula f2
  TEqual f1 f2 -> toFormula f1 := toFormula f2
  TNot f -> Not (toFormula f)

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
var = fmap TVar identifier <?> "variable"
table = [[Prefix ((reservedOp "~" >> return TNot) <?> "negation")],
         [op "/\\" TAnd AssocLeft, op "\\/" TOr AssocLeft],
         [op "=>" TImpl AssocRight, op "<=" TConseq AssocLeft],
         [op "==" TEq AssocLeft, op "=" TEqual AssocNone]]
        where op s f =
                  Infix ((reservedOp s >> return f) <?> "operator")

formula' = buildExpressionParser table term
term = parens formula'
       <|> var
       <|> (reserved "true" >> return TTrue)
       <|> (reserved "false" >> return TFalse)
       <?> "proposition"

formula = fmap toFormula formula'

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
