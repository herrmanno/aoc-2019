module Day17 () where

import IntCode (State, initialState,step,push,pop, runUntilOutput, run)
import IntCode qualified as IC
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)
import Data.List (find, intersperse, intercalate)

type Position = (Int,Int)

data Direction = U | R | D | L deriving (Show, Enum, Bounded)

fromChar :: Char -> Direction
fromChar '^' = U
fromChar 'v' = D
fromChar '<' = L
fromChar '>' = R

turnLeft :: Direction -> Direction
turnLeft = toEnum . (`mod` 4) . (+4) . pred . fromEnum

turnRight :: Direction -> Direction
turnRight = toEnum . (`mod` 4) . (+4) . succ . fromEnum

data Movement = TurnL | TurnR | Go Int deriving (Show, Eq)

toMovementCode :: Movement -> String
toMovementCode TurnL = "L"
toMovementCode TurnR = "R"
toMovementCode (Go x) = show x

newtype Path = Path { unPath :: [Movement] } deriving Show

newtype Pattern = Pattern { unPattern :: [Movement] } deriving Show

matches :: Pattern -> Path -> Bool
matches (Pattern xs) (Path ys) = let len = length xs in xs == take len ys

main = do
    content <- readFile "input.txt"
    let code = read $ "[" ++ content ++ "]" :: [Integer]
    let state1 = initialState code []
    let str = runProgram1 state1
    let map = toMap str
    -- let crossings = getCrossings map
    let totalCrossings = sumCrossings . getCrossings $ map
    -- let mapWithCrossings = foldr (`M.insert` 'X') map crossings
    putStrLn $ "Part 1: " ++ show totalCrossings
    let path = findPath map
    let patterns = findPatterns path
    let patternsApplied = convertToPatterns path patterns
    let output2 = runProgram2 code patterns patternsApplied
    putStrLn $ "Part 2: " ++ show output2

-- PART 1

runProgram1 :: State -> String
runProgram1 state =
    let (maybeOutput, state') = runUntilOutput state
    in case maybeOutput of
        Just out -> chr (fromIntegral out) : runProgram1 state'
        Nothing -> ""

toMap :: String -> Map Position Char
toMap str =
    let ls = filter (not . null) . lines $ str
        maxY = length ls - 1
        maxX = length (head ls) - 1
    in M.fromList [ ((x,y), (ls !! y) !! x) | x <- [0..maxX], y <- [0..maxY]]

-- toString :: Map Position Char -> String
-- toString m =
--     let maxY = maximum . fmap snd . M.keys $ m
--         maxX = maximum . fmap fst . M.keys $ m
--     in unlines [ [ m M.! (x,y) | x <- [0..maxX] ] | y <- [0..maxY]]

getCrossings :: Map Position Char -> [Position]
getCrossings map = filter (isCrossing map) . filter ((=='#') . (map M.!)) $ M.keys map
    where
        isCrossing m pos =
            let cells = fmap ((fromMaybe '.') . (m M.!?)) (neighbours pos)
                targetCells = filter (=='#') cells
            in length targetCells >= 3

sumCrossings :: [Position] -> Int
sumCrossings = sum . fmap (uncurry (*))

neighbours :: Position -> [Position]
neighbours (x,y) = [(x,y+1), (x,y-1), (x+1,y), (x-1,y)]

-- PART 2

runProgram2 :: [Integer] -> [Pattern] -> [Int] -> Integer
runProgram2 code patterns patternsApplied =
    let mainMovementRoutine = (++ "\n") . intersperse ',' . fmap (chr . (+ ord 'A')) $ patternsApplied
        movementFunctions = fmap ((++ "\n") . intercalate "," . fmap toMovementCode . unPattern) patterns
        continiousVideoFeed = "n\n"
        inputs = concat $ mainMovementRoutine : (movementFunctions ++ [continiousVideoFeed])
        rawInputs = fmap (fromIntegral . ord) $ inputs
        state = initialState (2 : tail code) rawInputs
        finalState = run state
    in head (IC.output finalState) -- IC output is in reverse order: newest at front

findPath :: Map Position Char -> Path
findPath m =
    let Just (startPos, startDir) = find ((`elem` "^v<>") . snd) $ M.toList m
    in Path . shortenPath $ go m startPos (fromChar startDir)
    where
        go :: Map Position Char -> Position -> Direction -> [Movement]
        go m pos dir =
            case (nextTile m pos dir, nextTile m pos (turnLeft dir), nextTile m pos (turnRight dir)) of
                ('#', _, _) -> Go 1 : go m (advance pos dir) dir
                (_, '#', _) -> TurnL : go m pos (turnLeft dir)
                (_, _, '#') -> TurnR : go m pos (turnRight dir)
                _           -> []
        
        shortenPath :: [Movement] -> [Movement]
        shortenPath [] = []
        shortenPath [x] = [x]
        shortenPath (Go a : Go b : xs) = shortenPath $ Go (a + b) : xs
        shortenPath (x:xs) = x : shortenPath xs

        nextTile :: Map Position Char -> Position -> Direction -> Char
        nextTile m pos dir = fromMaybe '.' (m M.!? advance pos dir)

        advance :: Position -> Direction -> Position
        advance (x,y) U = (x,y-1)
        advance (x,y) D = (x,y+1)
        advance (x,y) L = (x-1,y)
        advance (x,y) R = (x+1,y)

-- Breaks a path down into three (repeating) patterns, that describe the wohle path
findPatterns :: Path -> [Pattern]
findPatterns (Path path@(x:xs)) =
    let maxPatternLength = length path `div` 2 {- turn + movement -} `div` 3 {- num patterns -}
    in head
        [ [Pattern pat1, Pattern pat2, Pattern pat3]
        | len1 <- [maxPatternLength, maxPatternLength - 1 .. 1]
        , len2 <- [maxPatternLength, maxPatternLength - 1 .. 1]
        , len3 <- [maxPatternLength, maxPatternLength - 1 .. 1]
        , let (pat1, rest) = tryPattern len1 path
        , let (pat2, rest') = tryPattern len2 rest
        , let (pat3, rest'') = tryPattern len3 rest'
        , null rest''
        ]
    where
        tryPattern len path =
            let (pat,rest) = splitAt len path
            in (pat, cutPattern pat rest)
        cutPattern _ [] = []
        cutPattern pat path
            | Pattern pat `matches` Path path = cutPattern pat (drop (length pat) path)
            | otherwise                       = head path : cutPattern pat (tail path)

-- Translates a path into a series of applied patterns (identified by index)
convertToPatterns :: Path -> [Pattern] -> [Int]
convertToPatterns (Path []) _ = []
convertToPatterns path patterns =
    let idx = fmap fst . find ((`matches` path) . snd) $ zip [0..] patterns
    in case idx of
        Just i -> 
            let Pattern pat = patterns !! i
                path' = Path . drop (length pat) . unPath $ path
            in i : convertToPatterns path' patterns
        Nothing -> error "No pattern is matching current path "