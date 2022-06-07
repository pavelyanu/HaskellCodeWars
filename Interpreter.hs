{-# LANGUAGE ViewPatterns #-}

module Interpreter (module Interpreter) where
import Data.Maybe
import Prelude hiding (lookup, insert)
import Data.Map
import Control.Monad
import Control.Monad.Trans.State ( StateT(StateT), runState, evalStateT, execStateT )

data Expr = Const Constant
    | Variable String
    | Minus Expr Expr
    | UMinus Expr
    | Plus Expr Expr
    | Times Expr Expr
    | Div Expr Expr
    | Greater Expr Expr
    | GreaterEq Expr Expr
    | Less Expr Expr
    | LessEq Expr Expr
    | Equal Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | Not Expr
    | Look
    | XPosition
    | YPosition
    | Direction
    deriving Show

data Stmt = Assign String Expr
    | Seq Stmt Stmt
    | Condition Expr Stmt Stmt
    | While Expr Stmt
    | TurnLeftStmt
    | TurnRightStmt
    | MoveStmt
    | PunchStmt
    deriving Show

data Move = PunchMove | TurnLeftMove | TurnRightMove | MoveMove | NoMove deriving Show

type Heap = Map String Constant

data Memory = Memory Heap Move deriving Show

data Constant =  IntConst Integer | BoolConst Bool | None deriving (Show, Ord, Eq)

type Position = (Integer, Integer)

type Direction = Integer

type Board = [[Integer]]

data Game = Game {board :: Board, direction :: Direction, player1Pos :: Position, player2Pos :: Position}


-- #################### Game related functions ####################


emptyGame :: Game
emptyGame = Game {board=emptyBoard, direction=1, player1Pos=(0, 0), player2Pos=(3, 3)}

emptyBoard :: Board
emptyBoard =
    let
        a = replicate 4 0;
        c = let (x : xs) = a in 1 : xs;
        b = replicate 4 a;
    in c : b;


-- #################### Constant related functions ####################


constToInt :: Constant -> Integer
constToInt x = case x of
    IntConst a -> a
    BoolConst a -> toInteger (fromEnum a)
    None -> 0

constToBool :: Constant -> Bool
constToBool x = case x of
    IntConst a -> a /= 0
    BoolConst a -> a
    None -> False

fromInt :: Integer -> Constant
fromInt = IntConst

eitherIsNone :: Constant -> Constant -> Bool
eitherIsNone x y = x == None || y == None

isNone :: Constant -> Bool
isNone x = x == None

ifEitherIsNoneThenNothing :: (Constant -> Constant -> Constant) -> Constant -> Constant -> Maybe Constant
ifEitherIsNoneThenNothing f x y = if eitherIsNone x y then Nothing else Just $ f x y

constPlus :: Constant -> Constant -> Maybe Constant
constPlus = ifEitherIsNoneThenNothing (\x y -> fromInt (constToInt x + constToInt y))

constTimes :: Constant -> Constant -> Maybe Constant
constTimes = ifEitherIsNoneThenNothing (\x y -> fromInt (constToInt x * constToInt y))

constMinus :: Constant -> Constant -> Maybe Constant
constMinus = ifEitherIsNoneThenNothing (\x y -> fromInt (constToInt x - constToInt y))

constUMinus :: Constant -> Maybe Constant
constUMinus x = if isNone x then Nothing else Just $ IntConst (constToInt x * (-1))

constOr :: Constant -> Constant -> Maybe Constant
constOr = ifEitherIsNoneThenNothing (\x y -> BoolConst $ constToBool x || constToBool y)

constAnd :: Constant -> Constant -> Maybe Constant
constAnd = ifEitherIsNoneThenNothing (\x y -> BoolConst $ constToBool x && constToBool y)

constNot :: Constant -> Maybe Constant
constNot x = if isNone x then Nothing else Just $ BoolConst $ not $ constToBool x

constDiv :: Constant -> Constant -> Maybe Constant
constDiv = ifEitherIsNoneThenNothing (\x y -> IntConst $ constToInt x `div` constToInt y)

constGreater :: Constant -> Constant -> Maybe Constant
constGreater = ifEitherIsNoneThenNothing (\x y -> BoolConst $ constToInt x > constToInt y)

constLess :: Constant -> Constant -> Maybe Constant
constLess = ifEitherIsNoneThenNothing (\x y -> BoolConst $ constToInt x < constToInt y)

constEqual :: Constant -> Constant -> Maybe Constant
constEqual = ifEitherIsNoneThenNothing (\x y -> BoolConst $ constToInt x == constToInt y)

constGreaterEq :: Constant -> Constant -> Maybe Constant
constGreaterEq = ifEitherIsNoneThenNothing (\x y -> BoolConst $ constToInt x >= constToInt y)

constLessEq :: Constant -> Constant -> Maybe Constant
constLessEq = ifEitherIsNoneThenNothing (\x y -> BoolConst $ constToInt x <= constToInt y)


-- #################### Memory related functions ####################


emptyMemory :: Memory
emptyMemory = Memory empty NoMove

insertM :: String -> Constant -> StateT Memory Maybe ()
insertM k v = StateT $ \(Memory s m) -> Just ((), Memory (insert k v s) m)

lookupM :: String -> StateT Memory Maybe Constant
lookupM k = StateT $ \(Memory s m) -> let (const, state) = (fromMaybe None (lookup k s), s)
    in if const == None then Nothing
        else Just (const, Memory s m)

evalPairOfExpr :: Game -> (Expr, Expr) -> StateT Memory Maybe (Constant, Constant)
evalPairOfExpr game (x, y) = do
    r0 <- eval game x
    r1 <- eval game y
    return (r0, r1)

returnMaybe :: Maybe Constant -> StateT Memory Maybe Constant
returnMaybe x = StateT $ \s -> if isNothing x then Nothing else Just (fromMaybe None x, s)

putMove :: Move -> StateT Memory Maybe ()
putMove move = StateT $ \(Memory s m) -> Just ((), Memory s move)

getXPosition :: Game -> StateT Memory Maybe Constant
getXPosition game = StateT $ \s -> let (x, y) = player1Pos game in Just (IntConst x, s)

getYPosition :: Game -> StateT Memory Maybe Constant
getYPosition game = StateT $ \s -> let (x, y) = player1Pos game in Just (IntConst y, s)

getDirection :: Game -> StateT Memory Maybe Constant
getDirection game = StateT $ \s -> let d = direction game in Just (IntConst d, s)

getLook :: Game -> StateT Memory Maybe Constant
getLook game = StateT $ \s -> case () of
    _ | d == 1 && x1 == x2 && y1 - y2 > 0 -> Just (IntConst $ y1 - y2, s)
      | d == 3 && x1 == x2 && y1 - y2 < 0 -> Just (IntConst $ y2 - y1, s)
      | d == 2 && y1 == y2 && x1 - x2 < 0 -> Just (IntConst $ x2 - x1, s)
      | d == 4 && y1 == y2 && x1 - x2 > 0 -> Just (IntConst $ x1 - x2, s)
      | otherwise -> Just (IntConst (-1), s)
    where
        (x1, y1) = player1Pos game
        (x2, y2) = player2Pos game
        d = direction game


-- #################### Expression evaluation ####################


eval :: Game -> Expr -> StateT Memory Maybe Constant
eval game exp = case exp of
    Const x -> return x
    Variable x -> lookupM x
    Minus x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constMinus r0 r1)
    UMinus x ->
        do
            r0 <- eval game x
            returnMaybe (constUMinus r0)
    Plus x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constPlus r0 r1)
    Times x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constTimes r0 r1)
    Div x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constDiv r0 r1)
    Greater x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constGreater r0 r1)
    GreaterEq x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constGreaterEq r0 r1)
    Less x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constLess r0 r1)
    LessEq x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constLessEq r0 r1)
    Equal x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constEqual r0 r1)
    And x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constAnd r0 r1)
    Or x y ->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            returnMaybe (constOr r0 r1)
    Not x ->
        do
            r0 <- eval game x
            returnMaybe (constNot r0)
    Look -> getLook game
    XPosition -> getXPosition game
    YPosition -> getYPosition game
    Direction -> getDirection game


-- #################### Statement interpretation ####################


interpret :: Game -> Stmt -> StateT Memory Maybe ()
interpret game stmt = case stmt of
    Assign n e ->
        do
            r0 <- eval game e
            insertM n r0
    Seq x y ->
        do
            r0 <- interpret game x
            r1 <- interpret game y
            return ()
    Condition e x y ->
        do
            r0 <- eval game e
            if constToBool r0
                then interpret game x
                else interpret game y
    While e x ->
        let loop () = do
            r0 <- eval game e
            when (constToBool r0) $ do
                    interpret game x
                    loop ()
        in loop ()
    TurnLeftStmt -> putMove TurnLeftMove
    TurnRightStmt -> putMove TurnRightMove
    MoveStmt -> putMove MoveMove
    PunchStmt -> putMove PunchMove


-- #################### Testing ####################


testExpression = Equal
    (Greater (Variable "x") (Const $ IntConst 5))
    (Const $ BoolConst False)

testStatement = Seq
    (Assign "x" (Const $ IntConst 0))
    (While (Not $ Equal (Variable "x") (Const $ IntConst 5)) (Assign "x" (Plus (Variable "x") (Const $ IntConst 1))))


test :: Game -> StateT Memory Maybe ()
test game = do
    r0 <- interpret game testStatement
    return ()

printTest = execStateT (test emptyGame) emptyMemory
