import IntCode (State, initialState,step,push,pop, runUntilOutput)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)

type Position = (Int,Int)
type Path = [Int]
-- | 0: Wall, 1: Space, 2: Target
type Result = Int
-- | 1: North, 2: South, 3: West, 4: East
type Direction = Int

main = do
    content <- readFile "input.txt"
    let code = read $ "[" ++ content ++ "]" :: [Integer]
    let state = initialState code []
    let (map,path) = findMap state
    putStrLn $ showMap map (Just (0,0))
    putStrLn $ "Part 1: " ++ show (length path)
    let time = fillMapWithOxygen map (calculatePathTarget path)
    putStrLn $ "Part 2: " ++ show time

-- | Fills map with oxygen, starting at 'pos'. Returns the time it takes to fill the whole map
fillMapWithOxygen :: Map Position Char -> Position -> Int
fillMapWithOxygen m pos = go m [pos] 0
    where
        go :: Map Position Char -> [Position] -> Int -> Int
        go map positions t =
            let map' = foldr fillNeighboursWithOxygen map positions
                positions' = fmap fst . filter ((== 'O') . snd) . M.toList $ map'
            in
                if length positions == length positions'
                    then t
                    else go map' positions' (t + 1)
        -- | Fills all neighbours of 'pos', that are empty spaces (== '.') with oxygen ('O')
        fillNeighboursWithOxygen pos map =
            let freeNeighbourCells = filter ((=='.') . fromMaybe ' ' . (map M.!?)) (neighbours pos)
            in foldr (\p m -> M.insert p 'O' m) map freeNeighbourCells
        neighbours (x,y) = [(x,y+1), (x,y-1), (x+1,y), (x-1,y)]

-- | Created the complete map by bfs. Returns the map and the path to oxygen source.
findMap :: State -> (Map Position Char, Path)
findMap state = go [[x] | x <- [1..4]] (M.fromList [((0,0), '.')]) S.empty Nothing
    where
        go :: [Path] -> Map Position Char -> Set Position -> Maybe Path -> (Map Position Char, Path)
        go [] map _ targetPath = 
            case targetPath of
                Just path -> (map, path)
                _ -> error "Constructed complete map but did not find oxygen source"
        go (path:paths) map seen targetPath =
            let (position, result) = goPath state (0,0) path
                seen' = S.insert position seen
                newPaths = filter ((`S.notMember` seen') . calculatePathTarget) [ path ++ [x] | x <- [1..4]]
                paths' = paths ++ newPaths
            in
                case result of
                    2 ->
                        let map' = M.insert position 'O' map
                        in go paths' map' seen' (Just path)
                    1 ->
                        let map' = M.insert position '.' map
                        in go paths' map' seen' targetPath
                    0 ->
                        let map' = M.insert position '#' map
                        in go paths map' seen' targetPath

-- | Calculates a position by a path (starting at origin).
calculatePathTarget :: Path -> Position
calculatePathTarget path = go path (0,0)
    where
        go [] pos = pos
        go (1:xs) (x,y) = go xs (x, y - 1)
        go (2:xs) (x,y) = go xs (x, y + 1)
        go (3:xs) (x,y) = go xs (x + 1, y)
        go (4:xs) (x,y) = go xs (x - 1, y)

-- | Walks a path starting at 'pos'. Returns the final position and result (floor type at that position)
goPath :: State -> Position -> Path -> (Position, Result)
goPath state pos (x:xs) =
    let (state', pos', result) = go state x pos
    in
        -- traceShow (x:xs) $
        case result of
            0 | not (null xs)   -> error $ "Reached wall but path is not at end. At position: " ++ show pos' ++ ". Remaining path: " ++ show xs
            _ | null xs         -> (pos', result)
            _                   -> goPath state' pos' xs
    where
        go :: State -> Direction -> Position -> (State, Position, Result)
        go state direction pos@(x,y) =
            let (state', result) = goStep state direction
                pos' =
                    case direction of
                        1 -> (x, y - 1)
                        2 -> (x, y + 1)
                        3 -> (x + 1, y)
                        4 -> (x - 1, y)
            in (state', pos', result)

-- | Walks a single step.
goStep :: State -> Direction -> (State, Result)
goStep state direction =
    let (Just output, state') = runUntilOutput . push (fromIntegral direction) $ state
    in (state', fromIntegral output)

showMap :: Map Position Char -> Maybe Position -> String
showMap m pos =
    let minX = minimum (fmap fst $ M.keys m)
        maxX = maximum (fmap fst $ M.keys m)
        minY = minimum (fmap snd $ M.keys m)
        maxY = maximum (fmap snd $ M.keys m)
        getChar (x,y)
            | pos == Just (x,y) = '.'
            | otherwise = fromMaybe ' ' (m M.!? (x,y))
        lines = [ [ getChar (x,y) | x <- [minX..maxX]] | y <- [minY..maxY]]
    in unlines lines