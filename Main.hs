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

setMemory :: Index -> Memory -> GameMemory -> GameMemory
setMemory i m (GameMemory g m1 m2 c1 c2) = if i == 1
    then GameMemory g m m2 c1 c2
    else GameMemory g m1 m c1 c2

code :: IO String
code = readFile "code.wars"

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
                    else run newState

main :: IO ()
main = do
    f1 : f2 : duration : _ <- getArgs
    c1 <- readFile f1
    c2 <- readFile f2
    let state = GameMemory {
        game = newGame 3 10 0 (0, 0) (2, 2) 5,
        memory1 = emptyMemory 1,
        memory2 = emptyMemory 2,
        code1 = parseString c1,
        code2 = parseString c2
        }
        in run state

