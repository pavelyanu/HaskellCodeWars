module WarGame (module WarGame) where
import Data.Maybe

type Position = (Integer, Integer)

type Damage = Integer

type Direction = Integer

type Index = Integer

type Health = Integer

type BoardSize = Integer

type MaxTime = Integer

type CurrentTime = Integer

newtype Board = Board [[Integer]]

type Distance = Integer

data Move = PunchMove | TurnLeftMove | TurnRightMove | MoveMove | NoMove deriving Show

data Player = Player {index:: Index, direction :: Direction, position :: Position, health :: Health}

data Game = Game {
    board :: Board,
    player1 :: Player,
    player2 :: Player,
    currentTime :: Integer,
    maxTime :: Integer
    }


-- #################### Constructor functions ####################


defaultPlayer :: Index -> Health -> Position -> Player
defaultPlayer i hlth pos = Player {
    index = i,
    direction = 1,
    position=pos,
    health = hlth
    }

emptyBoard :: Integer -> Integer -> Board
emptyBoard 0 _ = Board []
emptyBoard _ 0 = Board []
emptyBoard width hight = Board (replicate (fromEnum width) 0 : remainder)
    where (Board remainder) = emptyBoard width (hight - 1)

newGame :: BoardSize -> MaxTime -> CurrentTime -> Position -> Position -> Health -> Game
newGame size maxT curT pos1 pos2 hlth = Game {
    board = brd,
    player1 = pl1,
    player2 = pl2,
    currentTime = curT,
    maxTime = maxT
    }
    where
        brd0 = emptyBoard size size
        pl1 = defaultPlayer 1 hlth pos1
        pl2 = defaultPlayer 2 hlth pos2
        brd = placePlayerOnBoard pl2 (placePlayerOnBoard pl1 brd0)


-- #################### Interacting with Game ####################


placeOnBoard :: Integer -> Position -> Board -> Board
placeOnBoard p (c, r) (Board board) = Board (as ++ [bs1 ++ [p] ++ bs2] ++ cs)
    where
        (as, bs : cs) = splitAt (fromEnum r) board
        (bs1, b : bs2) = splitAt (fromEnum c) bs

placePlayerOnBoard :: Player -> Board -> Board
placePlayerOnBoard (Player i _ pos _) = placeOnBoard i pos

updateGame :: Player -> Player -> Game -> Game
updateGame pl1 pl2 (Game (Board brd0) _ _ curT maxT) = Game brd3 pl1 pl2 curT maxT
    where
        brd1 = emptyBoard (toInteger $ length brd0) (toInteger $ length brd0)
        brd2 = placePlayerOnBoard pl1 brd1
        brd3 = placePlayerOnBoard pl2 brd2

updateOnePlayer :: Player -> Index -> Game -> Game
updateOnePlayer p i g = updateGame pl1 pl2 g
    where
        (pl1, pl2) = if i == 1 then (p, player2 g) else (player1 g, p)

incrementTime :: Game -> Game
incrementTime (Game a b c curT d) = Game a b c (curT + 1) d

timeUp :: Game -> Bool
timeUp g = currentTime g >= maxTime g

isFinished :: Game -> Bool
isFinished g = timeUp g || not (isAlive $ player1 g) || not (isAlive $ player2 g)

printWinner :: Game -> String
printWinner g 
    | (isAlive $ player1 g) && not (isAlive $ player2 g) = "Player 1 has won. (ง'̀-'́)ง"
    | not (isAlive $ player1 g) && (isAlive $ player2 g) = "Player 2 has won. ᕙ(⇀‸↼‶)ᕗ"
    | not (isAlive $ player1 g) && not (isAlive $ player2 g) = "Both lost. ¯\\_(ツ)_/¯"
    | timeUp g = "Time's up. (ʘᗩʘ')"
    | otherwise = "Game is not finished yet"

getBoardSize :: Game -> BoardSize
getBoardSize g = toInteger $ length b
    where Board b = board g

getLook :: Index -> Game -> Distance
getLook i game
    | d == 1 && x1 == x2 && y1 - y2 > 0 = y1 - y2
    | d == 3 && x1 == x2 && y1 - y2 < 0 = y2 - y1
    | d == 2 && y1 == y2 && x1 - x2 < 0 = x2 - x1
    | d == 4 && y1 == y2 && x1 - x2 > 0 = x1 - x2
    | otherwise = -1
        where
            (pl, tar) = getActorAndTarget i game
            (x1, y1) = position pl
            (x2, y2) = position tar
            d = direction pl

getPos :: Index -> Game -> Position
getPos i g = position (if i == 1 then player1 g else player2 g)

getDir :: Index -> Game -> Direction
getDir i g = direction (if i == 1 then player1 g else player2 g)

getPlayerByIndex :: Index -> Game -> Player
getPlayerByIndex i g = if i == 1 then player1 g else player2 g

getOppositePlayer :: Index -> Game -> Player
getOppositePlayer i g = if i == 1 then player2 g else player1 g

getActorAndTarget :: Index -> Game -> (Player, Player)
getActorAndTarget i g = if i == 1 then (player1 g, player2 g) else (player2 g, player1 g)

-- #################### Processing Moves ####################

processPunchMove :: Index -> Game -> Game
processPunchMove actor g = updateGame pl1 pl2 g
    where 
        (pl, tar0) = getActorAndTarget actor g
        attackPosition = fromMaybe (-1, -1) (getSquareInDirection (position pl) (direction pl) g)
        tar1 = if attackPosition == position tar0 then damagePlayer tar0 1 else tar0
        (pl1, pl2) = if actor == 1 then (pl, tar1) else (tar1, pl)

processMovement :: Index -> Move -> Game -> Game
processMovement actor move g = updateOnePlayer pl actor g
    where
        p = getPlayerByIndex actor g
        pl = case move of
            MoveMove -> movePlayer p g
            TurnLeftMove -> turnPlayerLeft p
            TurnRightMove -> turnPlayerRigth p
            PunchMove -> p
            NoMove -> p

processMoves :: (Move, Move) -> Game -> Game
processMoves (m1, m2) g = incrementTime g'
    where
        g' = case (m1, m2) of
            (PunchMove, PunchMove) -> processPunchMove 2 (processPunchMove 1 g)
            (PunchMove, move2) -> processMovement 2 move2 (processPunchMove 1 g)
            (move1, PunchMove) -> processMovement 1 move1 (processPunchMove 2 g)
            (move1, move2) -> processMovement 1 move1 (processMovement 2 move2 g)


-- #################### Interacting with Player ####################


movePlayer :: Player -> Game -> Player
movePlayer (Player i d p h) g = Player i d newPos h
    where
        newPos' = fromMaybe p (getSquareInDirection p d g)
        enemyPos = getOppositePlayer i g
        newPos = if newPos' == position enemyPos
            then p
            else newPos'

turnPlayerLeft :: Player -> Player
turnPlayerLeft (Player i d p h) = Player i newDir p h 
    where
        newDir' = (d - 1) `mod` 5
        newDir = if newDir' == 0 then 4 else newDir'

turnPlayerRigth :: Player -> Player
turnPlayerRigth (Player i d p h) = Player i newDir p h 
    where
        newDir' = (d + 1) `mod` 5
        newDir = if newDir' == 0 then 1 else newDir'

damagePlayer :: Player -> Damage -> Player
damagePlayer (Player i d p h) damage = Player i d p (h - damage)

isAlive :: Player -> Bool
isAlive p = health p > 0


-- #################### Interacting with Position ####################


getSquareInDirection :: Position -> Direction -> Game -> Maybe Position
getSquareInDirection (x, y) d g = if
    resX >= boardSize ||
    resY >= boardSize ||
    resX < 0 || resY < 0
    then Nothing else Just (resX, resY)
    where
        boardSize = getBoardSize g
        (dx, dy)
            | d == 1 = (0, -1)
            | d == 2 = (1, 0)
            | d == 3 = (0, 1)
            | d == 4 = (-1, 0)
            | otherwise = (0, 0)
        (resX, resY) = (x + dx, y + dy)


-- #################### Implementations ####################


showDir :: Direction -> String
showDir d
    | d == 1 = "Up"
    | d == 2 = "Right"
    | d == 3 = "Down"
    | d == 4 = "Left"
    | otherwise = "error"

instance Show Player where
    show p = 
        "Player "
        ++ show (index p)
        ++ ": health: "
        ++ show (health p)
        ++ "; direction: "
        ++ showDir (direction p)
        ++ "; position: "
        ++ show (position p)
        ++ " "

instance Show Board where
    show (Board b) = map (\x -> if x == '0' then '_' else x) (showBoard b)

showBoard :: [[Integer]] -> String
showBoard [] = ""
showBoard (b : bs) = showRow b ++ "\n" ++ showBoard bs
showRow :: [Integer] -> String
showRow = concatMap show

instance Show Game where
    show g = show (board g)
        ++ show (player1 g)
        ++ "\n"
        ++ show (player2 g)
        ++ "\n"

