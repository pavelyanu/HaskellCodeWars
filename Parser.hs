{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Parser (module Parser) where
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity
import Interpreter
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Prelude hiding (sequence)

names =
    words
    "True False If Then Else While Look XPosition YPosition Direction TurnRight TurnLeft Move Punch None"

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
    Const (BoolConst True) <$ reserved "True"
    <|> Const (BoolConst False) <$ reserved "False"

variable :: Parser Expr
variable = Variable <$> identifier

none :: Parser Expr
none = do
    reserved "None"
    return (Const None)

look :: Parser Expr
look = do
    reserved "Look"
    return Look

xPos :: Parser Expr
xPos = do
    reserved "XPosition"
    return XPosition

yPos :: Parser Expr
yPos = do
    reserved "YPosition"
    return YPosition

direction :: Parser Expr
direction = do
    reserved "Direction"
    return Direction

func :: Parser Expr
func = look
    <|> xPos
    <|> yPos
    <|> direction
    <|> none

term :: Parser Expr
term =
    intConst
    <|> func
    <|> boolConst
    <|> variable
    <|> parens expression

expression :: Parser Expr
expression = buildExpressionParser operationTable term <|> term

assignment :: Parser Stmt
assignment = do
    v0 <- identifier
    reservedOp "="
    Assign v0 <$> expression

sequence :: Parser Stmt
sequence = do
    s0 <- simpleStmt
    Seq s0 <$> stmt

condition :: Parser Stmt
condition = do
    reserved "If"
    e0 <- parens expression
    s0 <- between (reserved "{") (reserved "}") stmt
    reserved "Else"
    s1 <- between (reserved "{") (reserved "}") stmt
    return (Condition e0 s0 s1)

while :: Parser Stmt
while = do
    reserved "While"
    e0 <- parens expression
    s0 <- between (reserved "{") (reserved "}") stmt
    return (While e0 s0)

turnLeftStmt :: Parser Stmt
turnLeftStmt = do
    reserved "TurnLeft"
    return TurnLeftStmt

turnRightStmt :: Parser Stmt
turnRightStmt = do
    reserved "TurnRight"
    return TurnRightStmt

moveStmt :: Parser Stmt
moveStmt = do
    reserved "Move"
    return MoveStmt

punchStmt :: Parser Stmt
punchStmt = do
    reserved "Punch"
    return PunchStmt

simpleStmt :: Parser Stmt
simpleStmt = do
    s0 <- assignment
        <|> turnLeftStmt
        <|> turnRightStmt
        <|> moveStmt
        <|> punchStmt
        <|> while
        <|> condition
    semi
    return s0

stmt :: Parser Stmt
stmt = do
    try sequence <|> simpleStmt

parser = whiteSpace >> stmt

parseString :: String -> Stmt
parseString string =
    case parse parser "" string of
        Left e -> error $ show e
        Right r -> r
