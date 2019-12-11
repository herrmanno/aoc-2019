module Main where

import qualified Data.Map as M
import qualified IntCode as IC
import IntCode (initialState,runWhile,push,pop)
import Robot

main = do
    content <- readFile "input.txt"
    let code = read $ "[" ++ content ++ "]"
    let icm = initialState code [0]
    let board = M.empty
    let robot = (0, 0, (0, -1))
    let board' = moveRobot icm board robot
    print $ M.size board'
