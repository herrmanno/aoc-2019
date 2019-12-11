module Main where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified IntCode as IC
import IntCode (initialState,runWhile,push,pop)
import Robot

minKeys b = let
    x = foldl1 min $ map fst $ M.keys b
    y = foldl1 min $ map snd $ M.keys b
    in (x,y)

maxKeys b = let
    x = foldl1 max $ map fst $ M.keys b
    y = foldl1 max $ map snd $ M.keys b
    in (x,y)

transpose _ [] = []
transpose (a,b) (l:ls) = let
    ((x,y), c) = l
    l' = ((x + a, y + b), c)
    in l':(transpose (a,b) ls)

printboard b = let
    (xM, yM) = maxKeys b
    in printboard' b xM yM

printboard' :: Board -> Int -> Int -> IO ()
printboard' b x (-1) = return ()
printboard' b x y = do
    let (_, max) = maxKeys b
    let line = concat $ map (toS . (fromMaybe 0) . (`M.lookup` b)) $ zip [0..(x-1)] (repeat (max - y))
    print line
    printboard' b x (y - 1)
    where
        toS 1 = "##"
        toS _ = "  "


main = do
    content <- readFile "input.txt"
    let code = read $ "[" ++ content ++ "]"
    let icm = initialState code [1]
    let board = M.empty
    let robot = (0, 0, (0, -1))
    let board' = moveRobot icm board robot
    -- let (minX,minY) = minKeys board'
    -- let ls = M.toList board'
    -- let ls' = transpose (minY,minY) ls
    -- let boardT = M.fromList ls'
    printboard board'
    -- print $ ls'
    