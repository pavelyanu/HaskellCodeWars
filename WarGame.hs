type Position = (Integer, Integer)

type Direction = Integer

type Index = Integer

type Health = Integer

type BoardSize = Integer

type MaxTime = Integer

type CurrentTime = Integer

newtype Board = Board [[Integer]]

data Player = Player {index:: Integer, direction :: Direction, position :: Position, health :: Health}

data Game = Game {
    board :: Board,
    player1 :: Player,
    player2 :: Player,
    currentTime :: Integer,
    maxTime :: Integer
    }

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

placeOnBoard :: Integer -> Position -> Board -> Board
placeOnBoard p (c, r) (Board board) = Board (as ++ [bs1 ++ [p] ++ bs2] ++ cs)
    where
        (as, bs : cs) = splitAt (fromEnum r) board
        (bs1, b : bs2) = splitAt (fromEnum c) bs

placePlayerOnBoard :: Player -> Board -> Board
placePlayerOnBoard (Player i _ pos _) = placeOnBoard i pos

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

updateGame :: Player -> Player -> Game -> Game
updateGame pl1 pl2 (Game (Board brd0) _ _ curT maxT) = Game brd3 pl1 pl2 curT maxT
    where
        brd1 = emptyBoard (toInteger $ length brd0) (toInteger $ length brd0)
        brd2 = placePlayerOnBoard pl1 brd1
        brd3 = placePlayerOnBoard pl2 brd2

instance Show Board where
    show (Board b) = map (\x -> if x == '0' then '_' else x) (showBoard b)

showBoard :: [[Integer]] -> String
showBoard [] = ""
showBoard (b : bs) = showRow b ++ "\n" ++ showBoard bs
showRow :: [Integer] -> String
showRow = concatMap show

instance Show Game where
    show g = show (board g)
        ++ "Player 1: "
        ++ show (health $ player1 g)
        ++ "Player 2: "
        ++ show (health $ player2 g)
