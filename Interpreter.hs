{-# LANGUAGE ViewPatterns #-}

module Interpreter (module Interpreter) where
import Data.Maybe
import Prelude hiding (lookup, insert)
import Data.Map
import Control.Monad
import Control.Monad.Trans.State ( StateT(StateT), runState, evalStateT, execStateT, get, gets, modify )
import System.Random
import WarGame

data Expr = Const Constant
    | Variable String
    | Rand
    | UnOp Op Expr
    | BinOp Op Expr Expr
    | Look
    | XPosition
    | YPosition
    | Direction
    deriving Show

data Stmt = Assign String Expr
    | Seq Stmt Stmt
    | IfElse Expr Stmt Stmt
    | If Expr Stmt
    | While Expr Stmt
    -- | End
    | TurnLeftStmt
    | TurnRightStmt
    | MoveStmt
    | PunchStmt
    deriving Show

type Heap = Map String Constant

type Seed = Integer

data Memory = Memory {heap :: Heap, move :: Move, index :: Index, seed :: Seed} deriving Show

data Constant =  IntConst Integer | BoolConst Bool | None deriving (Show, Ord, Eq)

data Op = UMinus | Plus | Minus | Times | Div | Mod | Greater | GreaterEq | Less | LessEq | Equal | And | Or | Not deriving (Show, Ord, Eq)

-- #################### Constant related functions ####################

unOpTable = fromList [
    (UMinus, \x -> IntConst (constToInt x * (-1))),
    (Not, BoolConst . not . constToBool)
    ]

binOpTable = fromList [
    (Plus, \x y -> fromIntegerToConst (constToInt x + constToInt y)),
    (Minus, \x y -> fromIntegerToConst (constToInt x - constToInt y)),
    (Times, \x y -> fromIntegerToConst (constToInt x * constToInt y)),
    (Div, \x y -> IntConst $ constToInt x `div` constToInt y),
    (Mod, \x y -> IntConst $ constToInt x `mod` constToInt y),
    (Greater, \x y -> BoolConst $ constToInt x > constToInt y),
    (GreaterEq, \x y -> BoolConst $ constToInt x >= constToInt y),
    (Less, \x y -> BoolConst $ constToInt x < constToInt y),
    (LessEq, \x y -> BoolConst $ constToInt x <= constToInt y),
    (And, \x y -> BoolConst $ constToBool x && constToBool y),
    (Or, \x y -> BoolConst $ constToBool x || constToBool y)
    ]

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

fromIntegerToConst :: Integer -> Constant
fromIntegerToConst = IntConst

eitherIsNone :: Constant -> Constant -> Bool
eitherIsNone x y = x == None || y == None

isNone :: Constant -> Bool
isNone x = x == None

ifEitherIsNoneThenNothing :: (Constant -> Constant -> Constant) -> Constant -> Constant -> Maybe Constant
ifEitherIsNoneThenNothing f x y = if eitherIsNone x y then Nothing else Just $ f x y

ifNoneThenNothing :: (Constant -> Constant) -> Constant -> Maybe Constant
ifNoneThenNothing f x = if isNone x then Nothing else Just $ f x

constEqual :: Constant -> Constant -> Maybe Constant
constEqual x y
    | x == None && y == None = Just $ BoolConst True
    | x == None || y == None = Just $ BoolConst False
    | otherwise = Just (BoolConst $ constToInt x == constToInt y)

-- #################### Memory related functions ####################


emptyMemory :: Index -> Seed -> Memory
emptyMemory = Memory empty NoMove

setSeed :: Seed -> Memory -> Memory
setSeed newSeed (Memory h m i s) = Memory h m i newSeed

insertM :: String -> Constant -> StateT Memory Maybe ()
insertM k v = StateT $ \(Memory h m i s) -> Just ((), Memory (insert k v h) m i s)

lookupM :: String -> StateT Memory Maybe Constant
lookupM k = StateT $ \(Memory h m i s) -> let const = fromMaybe None (lookup k h)
    in Just (const, Memory h m i s)

evalPairOfExpr :: Game -> (Expr, Expr) -> StateT Memory Maybe (Constant, Constant)
evalPairOfExpr game (x, y) = do
    r0 <- eval game x
    r1 <- eval game y
    return (r0, r1)

returnMaybe :: Maybe Constant -> StateT Memory Maybe Constant
returnMaybe x = StateT $ \s -> if isNothing x then Nothing else Just (fromMaybe None x, s)

putMove :: Move -> StateT Memory Maybe ()
putMove move = StateT $ \(Memory h m i s) -> Just ((), Memory h move i s)

evalRandom :: StateT Memory Maybe Constant
evalRandom = do
    seed <- gets seed
    let (r, _) = random (mkStdGen $ fromEnum seed) in do
    _ <- modify $ setSeed (seed + 1)
    return $ IntConst r

getXPosition :: Game -> StateT Memory Maybe Constant
getXPosition game = StateT $ \(Memory h m i s) -> let (x, y) = getPos i game in
    Just (IntConst x, Memory h m i s)

getYPosition :: Game -> StateT Memory Maybe Constant
getYPosition game = StateT $ \(Memory h m i s) -> let (x, y) = getPos i game in
    Just (IntConst y, Memory h m i s)

getDirection :: Game -> StateT Memory Maybe Constant
getDirection game = StateT $ \(Memory h m i s) -> let d = getDir i game in
    Just (IntConst d, Memory h m i s)

getLookDist :: Game -> StateT Memory Maybe Constant
getLookDist game = StateT $ \(Memory h m i s) -> let dist = getLook i game in
    Just (IntConst dist, Memory h m i s)

-- #################### Expression evaluation ####################


eval :: Game -> Expr -> StateT Memory Maybe Constant
eval game exp = case exp of
    Const x -> return x
    Variable x -> lookupM x
    Rand -> evalRandom
    BinOp op x y->
        do
            (r0, r1) <- evalPairOfExpr game (x, y)
            if op == Equal
                then returnMaybe (constEqual r0 r1)
                else returnMaybe (ifEitherIsNoneThenNothing (fromJust (lookup op binOpTable)) r0 r1)
    UnOp op x ->
        do
            r0 <- eval game x
            returnMaybe (ifNoneThenNothing (fromJust (lookup op unOpTable)) r0)
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
            interpret game x
            interpret game y
            return ()
    IfElse e x y ->
        do
            r0 <- eval game e
            if constToBool r0
                then interpret game x
                else interpret game y
    If e x ->
        do
            r0 <- eval game e
            when (constToBool r0) $ do
                interpret game x
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
