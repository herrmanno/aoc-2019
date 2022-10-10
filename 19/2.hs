import IntCode
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)

main = do
    input <- readFile "input.txt"
    let code = read $ "[" ++ input ++ "]" :: [Integer]
    -- testInput <- readFile "test.txt"
    -- let testMap = M.fromList
    --             $ [ ((x,y), char == '#')
    --               | (y,line) <- (zip [0..] (lines testInput))
    --               , (x,char) <- (zip [0..] line)
    --               ]
    -- let test = fromMaybe False . (testMap M.!?)
    let (x,y) = findRightTopSquareCoord (probe code) (0,0)
    print $ 10000 * x + y

findRightTopSquareCoord test (x,y) =
    if x - width >= 0 && testSquare test (x - width, y)
        then
            (x - width, y)
        else
            let right = test (x+1, y)
                down = test (x, y+1)
            in
                if right then findRightTopSquareCoord test (x+1,y)
                else if down then findRightTopSquareCoord test (x,y+1)
                else findRightTopSquareCoord test (x+1,y+1)
    where
        width = 99
        testSquare test (x,y) = 
            let coords = [(x + x', y + y') | x' <- [0..width], y' <- [0..width]]
            in and (fmap test coords)

probe :: [Integer] -> (Integer, Integer) -> Bool
probe code (x,y) = (==1) . head . output $ run (initialState code [x,y])

