import Parser
import Interpreter
import WarGame
import Control.Monad.Trans.State
import System.Environment (getArgs)
import Data.Maybe
import System.Random
import Data.Time.Clock

data GameMemory = GameMemory {
    game :: Game,
    memory1 :: Memory,
    memory2 :: Memory,
    code1 :: Stmt,
    code2 :: Stmt
    }

getMemory :: Index -> GameMemory -> Memory
getMemory i s = if i == 1 then memory1 s else memory2 s

getCode :: Index -> GameMemory -> Stmt
getCode i s = if i == 1 then code1 s else code2 s

extractMove :: Index -> GameMemory -> Move
extractMove i gm = m
    where m = move (getMemory i gm)

setMemory :: Index -> Memory -> GameMemory -> GameMemory
setMemory i m (GameMemory g m1 m2 c1 c2) = if i == 1
    then GameMemory g m m2 c1 c2
    else GameMemory g m1 m c1 c2

setGame :: Game -> GameMemory -> GameMemory
setGame g (GameMemory _g m1 m2 c1 c2) = GameMemory g m1 m2 c1 c2

randomizeGame :: GameMemory -> Seed -> GameMemory
randomizeGame (GameMemory g m1' m2' c1 c2) seed = 
    let
        gen1 = mkStdGen $ fromEnum seed
        (dir1, gen2) = random gen1
        (dir2, _) = random gen2
        p1 = setDirection (player1 g) (dir1 `mod`  5 + 1)
        p2 = setDirection (player2 g) (dir2 `mod` 5 + 1)
        m1 = setSeed seed m1'
        m2 = setSeed (seed + 1) m2'
    in GameMemory (updateGame p1 p2 g) m1 m2 c1 c2 

makeGameMemory :: Stmt -> Stmt -> BoardSize -> MaxTime -> Seed -> GameMemory
makeGameMemory c1 c2 size duration seed = 
        let
            (d1, _) = random (mkStdGen $ fromEnum seed)
            (d2, _) = random (mkStdGen (fromEnum seed + 1))
        in GameMemory {
            game = newGame size duration 0 (0, 0) d1 (size - 1, size - 1) d2 4,
            memory1 = emptyMemory 1 seed,
            memory2 = emptyMemory 2 (seed + 1),
            code1 = c1,
            code2 = c2
            }

applyMoves :: StateT GameMemory Maybe ()
applyMoves = do
    move1 <- gets $ extractMove 1
    move2 <- gets $ extractMove 2
    oldGame <- gets game
    let newGame = processMoves (move1, move2) oldGame
    modify $ setGame newGame

runCode :: Index -> StateT GameMemory Maybe ()
runCode i = StateT $ \s -> let
    memory = getMemory i s
    code = getCode i s
    g = game s
    f newMemory = ((), setMemory i newMemory s)
    in f <$> execStateT (interpret g code) memory

runGame :: StateT GameMemory Maybe ()
runGame = do
    runCode 1
    runCode 2
    applyMoves

run :: GameMemory -> IO ()
run state = do
    case execStateT runGame state of
        Nothing -> error "Game momory is Nothing"
        Just newState -> do
            let stateOfGame = game newState
            putStr $ show stateOfGame
            if isFinished stateOfGame
                then putStr $ printWinner stateOfGame
                else do
                    let mem1 = memory1 newState
                        mem2 = memory2 newState
                    putStr $ show mem1
                    putStr "\n"
                    putStr $ show mem2
                    putStr "\n"
                    getLine
                    run newState

runSilently :: GameMemory -> IO Integer
runSilently state = case execStateT runGame state of
    Nothing -> error "Game memory is Nothing"
    Just newState -> do
        let stateOfGame = game newState
        if isFinished stateOfGame
            then return $ getWinner stateOfGame
            else do
                let mem1 = memory1 newState
                    mem2 = memory2 newState
                runSilently newState

runSimulation :: GameMemory -> Seed -> Integer -> Integer -> Integer -> IO (Integer, Integer)
runSimulation state seed rounds c1 c2 = do
    if rounds == 0 then return (c1, c2)
        else do
            winner <- runSilently (randomizeGame state seed)
            let (newC1, newC2) = case winner of
                    1 -> (c1 + 1, c2)
                    2 -> (c1, c2 + 1)
                    _ -> (c1, c2)
            if winner == 0
                then putStr "No one won\n"
                else putStr ("Player " ++ show winner ++ " has won the round\n")
            runSimulation state (seed + 1) (rounds - 1) newC1 newC2

main :: IO ()
main = do
    args <- getArgs
    time <- getCurrentTime
    if length args < 4 then printHelp else do
    let (f1 : f2 : duration' : size' : rest) = args
    c1 <- readFile f1
    c2 <- readFile f2
    let
        size = read size'
        duration = read duration'
        (seed, sim) = case rest of
            (seed' : sim' : _) -> (read seed', read sim')
            (seed' : _) -> (read seed', 0)
            _ -> (toInteger (fromEnum $ utctDayTime time) `mod` 10000, 0)
        code1 = parseString c1
        code2 = parseString c2
        state = makeGameMemory code1 code2 size duration seed
        in if sim == 0 then run state
            else do
                let rounds = sim
                (r1, r2) <- runSimulation state seed rounds 0 0
                putStr "Results of the simulation are:\n"
                putStr (
                    "Player 1: "
                    ++ show r1
                    ++ ", Player 2: "
                    ++ show r2 ++
                    ", No one won: "
                    ++ show (rounds - r1 - r2)
                    ++ "\n"
                    )

printHelp :: IO ()
printHelp = do
    putStr ("Correct arguments are:\n"
        ++ "[player1 code] [player2 code] [time limit] [board size] [optional seed] [optional number of rounds to simulate]\n")
    return ()
