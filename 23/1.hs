import IntCode
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl')
import Text.Printf (printf)
import Debug.Trace

type Message = (Integer, Integer, Integer)

main = do
    input <- readFile "input.txt"
    let code = read $ "[" ++ input ++ "]"
    let makeState = initialState code
    let (to, x, y) = go 0 (M.fromList [(i, makeState [fromIntegral i]) | i <- [0..l - 1]]) M.empty
    printf "Part 1: %d\n" y
    where
        l = 50
        go :: Int -> Map Int State -> Map Int [Message] -> Message
        go idx machines messages = -- traceShow (M.filter (not . null) messages) $
            let machine = machines M.! idx
                state' = runUntilInput machine
                newMessages = chunksOf 3 . reverse . output $ state'
                receivers = S.fromList [ fromIntegral to | [to, _, _] <- newMessages]
                messages' = foldl' (flip insertMessage) messages newMessages
                messagesForMachine = fromMaybe [] (messages' M.!? idx)
                state'' =
                    if null messagesForMachine
                        then push (-1) $ state'
                        else foldl' (flip push) state' (concatMap (tail . messageToList) messagesForMachine)
                messages'' = M.insert idx [] messages'
                machines' = M.insert idx state'' machines 
            in -- traceShow (idx, newMessages, M.filterWithKey (\k _ -> S.member k receivers) messages, M.filterWithKey (\k _ -> S.member k receivers) messages') $
                case messages M.!? 255 of
                    Just (x:_) -> x
                    _ -> go (succ idx `mod` l) machines' messages''

        insertMessage :: [Integer] -> Map Int [Message] -> Map Int [Message]
        insertMessage [to, x, y] messages =
            case messages M.!? (fromIntegral to) of
                Just xs -> M.insert (fromIntegral to) (xs ++ [(to, x,y)]) messages
                Nothing -> M.insert (fromIntegral to) [(to, x,y)] messages

        messageToList (to, x, y) = [to, x, y]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
    | length xs < n = error $ "Cannot divide list to chunks"
    | otherwise = take n xs : chunksOf n (drop n xs)

