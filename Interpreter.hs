{-# LANGUAGE ViewPatterns #-}

module Interpreter (module Interpreter) where
import Data.Maybe
import Prelude hiding (lookup, insert)
import Data.Map
import Control.Monad
import Control.Monad.Trans.State ( StateT(StateT), runState, evalStateT, execStateT )
import WarGame

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

type Heap = Map String Constant

data Memory = Memory {heap :: Heap, move :: Move, index :: Index} deriving Show

data Constant =  IntConst Integer | BoolConst Bool | None deriving (Show, Ord, Eq)

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
constEqual x y
    | x == None && y == None = Just $ BoolConst True
    | x == None || y == None = Just $ BoolConst False
    | otherwise = Just (BoolConst $ constToInt x == constToInt y)

constGreaterEq :: Constant -> Constant -> Maybe Constant
constGreaterEq = ifEitherIsNoneThenNothing (\x y -> BoolConst $ constToInt x >= constToInt y)

constLessEq :: Constant -> Constant -> Maybe Constant
constLessEq = ifEitherIsNoneThenNothing (\x y -> BoolConst $ constToInt x <= constToInt y)


-- #################### Memory related functions ####################


emptyMemory :: Index -> Memory
emptyMemory = Memory empty NoMove 

insertM :: String -> Constant -> StateT Memory Maybe ()
insertM k v = StateT $ \(Memory s m i) -> Just ((), Memory (insert k v s) m i)

lookupM :: String -> StateT Memory Maybe Constant
lookupM k = StateT $ \(Memory s m i) -> let (const, state) = (fromMaybe None (lookup k s), s)
    in Just (const, Memory s m i)

evalPairOfExpr :: Game -> (Expr, Expr) -> StateT Memory Maybe (Constant, Constant)
evalPairOfExpr game (x, y) = do
    r0 <- eval game x
    r1 <- eval game y
    return (r0, r1)

returnMaybe :: Maybe Constant -> StateT Memory Maybe Constant
returnMaybe x = StateT $ \s -> if isNothing x then Nothing else Just (fromMaybe None x, s)

putMove :: Move -> StateT Memory Maybe ()
putMove move = StateT $ \(Memory s m i) -> Just ((), Memory s move i)

getXPosition :: Game -> StateT Memory Maybe Constant
getXPosition game = StateT $ \(Memory s m i) -> let (x, y) = getPos i game in
    Just (IntConst x, Memory s m i)


getYPosition :: Game -> StateT Memory Maybe Constant
getYPosition game = StateT $ \(Memory s m i) -> let (x, y) = getPos i game in
    Just (IntConst y, Memory s m i)

getDirection :: Game -> StateT Memory Maybe Constant
getDirection game = StateT $ \(Memory s m i) -> let d = getDir i game in
    Just (IntConst d, Memory s m i)

getLookDist :: Game -> StateT Memory Maybe Constant
getLookDist game = StateT $ \(Memory s m i) -> let dist = getLook i game in
    Just (IntConst dist, Memory s m i)


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
    Look -> getLookDist game
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

-- printTest = execStateT (test emptyGame) emptyMemory
