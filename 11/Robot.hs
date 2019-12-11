module Robot where

import qualified IntCode as IC
import IntCode (initialState,runWhile,push,pop)
import qualified Data.Map as M
import Data.Maybe

type Color = Int

type Board = M.Map (Int,Int) Color

type Robot = (Int,Int,(Int,Int)) -- (x,y,dir) where dir = (xdiff,ydiff) <=> diff on next move

moveRobot :: IC.State -> Board -> Robot -> Board
moveRobot icm b r
    | (IC.halted icm) = b
    | otherwise = let
        (x,y,(xd,yd)) = r
        icm' = runWhile (\s -> (length $ IC.output s) < 2) icm
        (dir, icm'') = IC.pop icm'
        (color, icm''') = IC.pop icm''
        board' = M.insert (x,y) (fromIntegral color) b
        (xd',yd') = (turn dir (xd, yd))
        robot' = (x + xd', y + yd', (xd', yd'))
        (x',y',_) = robot'
        currColor = fromMaybe 0 $ M.lookup (x',y') board'
        icm'''' = IC.push (fromIntegral currColor) icm'''
        in moveRobot icm'''' board' robot'
        where
            turn 0 (x,y) = (y, negate x)
            turn 1 (x,y) = (negate y, x)
