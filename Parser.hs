{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity
import Interpreter
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

names = words "True False If Then Else While Look XPosition YPosition Direction"
opNames = words "&& || ! + - * / = < <= > >= =="

lexerConfig = emptyDef {
    Token.commentStart = "/*",
    Token.commentEnd = "*/",
    Token.commentLine = "//",
    Token.identStart = letter,
    Token.identLetter = alphaNum <|> char '_',
    Token.reservedNames = names,
    Token.reservedOpNames = opNames
}

lexer = Token.makeTokenParser lexerConfig

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
integer       = Token.integer       lexer
semi          = Token.semi          lexer
comma         = Token.comma         lexer
whiteSpace    = Token.whiteSpace    lexer

operationTable = [
    [ Prefix (reservedOp "-" >> return UMinus) ],
    [ Prefix (reservedOp "!" >> return Not) ],
    [
        Infix (reservedOp "*" >> return Times) AssocLeft,
        Infix (reservedOp "/" >> return Div) AssocLeft
    ],
    [
        Infix (reservedOp "+" >> return Plus) AssocLeft,
        Infix (reservedOp "-" >> return Minus) AssocLeft
    ],
    [ Infix (reservedOp "==" >> return Equal) AssocLeft ],
    [ Infix (reservedOp "<" >> return Less) AssocLeft ],
    [ Infix (reservedOp "<=" >> return LessEq) AssocLeft ],
    [ Infix (reservedOp ">" >> return Greater) AssocLeft ],
    [ Infix (reservedOp ">=" >> return GreaterEq) AssocLeft ],
    [ Infix (reservedOp "&&" >> return And) AssocLeft ],
    [ Infix (reservedOp "||" >> return Or) AssocLeft ]
    ]

intConst :: Parser Expr
intConst = Const . IntConst <$> integer

boolConst :: Parser Expr
boolConst = 
    do
        Const (BoolConst True) <$ reserved "True"
        <|> Const (BoolConst False) <$ reserved "False"

variable :: Parser Expr
variable = Variable <$> identifier

term :: Parser Expr
term =
    do
        intConst
        <|> boolConst
        <|> variable
        <|> parens expression 

expression :: Parser Expr
expression = 
    do
        buildExpressionParser operationTable term
        <|> term
