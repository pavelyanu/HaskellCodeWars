import Parser
import Interpreter
import WarGame
import Control.Monad.Trans.State
import System.Environment (getArgs)
import Data.Maybe

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

getMove :: Index -> GameMemory -> Move
getMove i gm = m
    where (Memory _ m _) = getMemory i gm

setMemory :: Index -> Memory -> GameMemory -> GameMemory
setMemory i m (GameMemory g m1 m2 c1 c2) = if i == 1
    then GameMemory g m m2 c1 c2
    else GameMemory g m1 m c1 c2

setGame :: Game -> GameMemory -> GameMemory
setGame g (GameMemory _g m1 m2 c1 c2) = GameMemory g m1 m2 c1 c2

applyMoves :: StateT GameMemory Maybe ()
applyMoves = StateT $ \s -> let
    move1 = getMove 1 s
    move2 = getMove 2 s
    oldGame = game s
    newGame = processMoves (move1, move2) oldGame
    in Just ((), setGame newGame s)

runCode :: Index -> StateT GameMemory Maybe ()
runCode i = StateT $ \s -> let
    memory = getMemory i s
    code = getCode i s
    g = game s
    maybeNewMemory = execStateT (interpret g code) memory
    in 
        if isNothing maybeNewMemory
            then Nothing
            else let newMemory = fromJust maybeNewMemory 
            in Just ((), setMemory i newMemory s)

runGame :: StateT GameMemory Maybe ()
runGame = do
    runCode 1
    runCode 2
    applyMoves

run :: GameMemory -> IO ()
run state = do
    let maybeNewState = execStateT runGame state
        in if isNothing maybeNewState
            then error "Game memory is Nothing"
            else do
                let newState = fromJust maybeNewState
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

main :: IO ()
main = do
    f1 : f2 : duration : size' : _ <- getArgs
    c1 <- readFile f1
    c2 <- readFile f2
    let
        size = read size'
        state = GameMemory {
        game = newGame (read size') (read duration) 0 (0, 0) (size - 1, size - 1) 4,
        memory1 = emptyMemory 1,
        memory2 = emptyMemory 2,
        code1 = parseString c1,
        code2 = parseString c2
        }
        in run state

