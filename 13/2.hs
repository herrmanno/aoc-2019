import IntCode (State (halted), initialState,runUntilInput,output, push, clear)
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Control.Concurrent ( threadDelay )

type BlockPosition = (Int,Int)
type BallPosition = (Int,Int)
type PaddlePosition = (Int, Int)
type Score = Int

type GameState = (Set BlockPosition, BallPosition, PaddlePosition, Score)

main = do
    content <- readFile "input.txt"
    let code = read $ "[" ++ content ++ "]"
    let code' = 2 : tail code
    let s = initialState code' []
    score <- run s [s] (S.empty, (0,0), (0,0), 0)
    putStr "\ESC[?25h" -- show cursor
    putStr "\ESC[2J" -- clear screen
    putStrLn $ "Final score: " ++ show score

run :: State -> [State] -> GameState -> IO Int
run state states gameState = do
    let state' = runUntilInput state
    let out = reverse . output $ state'
    let gameState'@(blockPositions, ballPosition, paddlePosition, score) = interpretOutput out gameState
    printOutput out
    threadDelay 5000 -- 5/1000s
    if S.null blockPositions
        then return score
    else if snd paddlePosition == snd ballPosition && fst paddlePosition /= fst ballPosition
        then
            let diff = fst ballPosition - fst paddlePosition
            {- The *unhappy* path. Let's just hope we never gonne have to backtrack... -}
            in undefined
        else case compare (fst ballPosition) (fst paddlePosition) of
                LT -> run (clear . push (-1) $ state') (state' : states) gameState'
                GT -> run (clear . push 1 $ state') (state' : states) gameState'
                EQ -> run (clear . push 0 $ state') (state' : states) gameState'

interpretOutput :: [Integer] -> GameState -> GameState
interpretOutput output state = foldr f state (chunksOf 3 output)
    where
        f [-1,0,score] (nb, bp, pp, sc) = (nb, bp, pp, fromIntegral score)
        f [x,y,2] (nb, bp, pp, sc) = (S.insert (fromInteger x,fromInteger y) nb, bp, pp, sc)
        f [x,y,3] (nb, bp, pp, sc) = (S.delete (fromInteger x, fromInteger y) nb, bp, (fromIntegral x, fromIntegral y), sc)
        f [x,y,4] (nb, bp, pp, sc) = (nb, (fromIntegral x,fromIntegral y), pp, sc)
        f [x,y,_] (nb, bp, pp, sc) = (S.delete (fromInteger x, fromInteger y) nb, bp, pp, sc)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    if length xs < n
        then error $ "Length of list is no multiple of " ++ show n
        else take n xs : chunksOf n (drop n xs)


printOutput :: [Integer] -> IO ()
printOutput output = do
    putStr "\ESC[?25l" -- hide cursor
    go output
    putStr "\n"
    where
        go [] = return ()
        go (-1:0:score:rest) = do
            putStr "\ESC[25;0H"
            putStr $ "\nScore: " ++ show score
            go rest
        go (x:y:c:rest) = do
            putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"
            putStr $ case c of
                0 -> " "
                1 -> "|"
                2 -> "█"
                3 -> "-"
                4 -> "*"
            go rest
        go xs = error $ "Got bad number of output elements: " ++ show xs