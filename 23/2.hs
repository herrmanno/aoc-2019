import IntCode
import Data.Map (Map)
import Data.Map qualified as M
import Data.Foldable (foldl')
import Text.Printf (printf)

type Message = [Integer]

main = do
    input <- readFile "input.txt"
    let code = read $ "[" ++ input ++ "]"
    let makeState = initialState code
    let machines = (M.fromList [(i, makeState [fromIntegral i]) | i <- [0..l - 1]])
    let [to, x, y] = go machines [] Nothing Nothing
    printf "Part 2: %d\n" y >> return ()
    where
        l = 50
        go :: Map Integer State -> [Message] -> Maybe Message -> Maybe Integer -> Message
        -- No messages enqueeud, no nat message available
        go machines [] Nothing lastNat =
            let indices = [0..l - 1]
                signalMachine (machines, messages) idx =
                    let (machine', messages') = signalEmptyQueue idx machines
                        result = (M.insert idx machine'  machines, messages ++ messages')
                    in result
                (machines', messages') = foldl' signalMachine (machines, []) indices
            in go machines' messages' Nothing lastNat
            where
                signalEmptyQueue idx machines =
                    let machine = machines M.! idx
                        machine' = runUntilInput (push (-1) machine)
                        newMessages = chunksOf 3 . reverse . output $ machine'
                        messages' = newMessages
                    in (machine', messages')

        -- Nat message delivered
        go machines [] (Just message@[_, x, y]) lastNat =
            let idx = 0
                machine = machines M.! idx
                machine' = runUntilInput (pushAll [x,y] machine)
                newMessages = chunksOf 3 . reverse . output $ machine'
                messages' = newMessages
                machines' = M.insert idx machine' machines
                natMessage' = Nothing
                lastNat' = Just y
            in
                if Just y == lastNat
                    then message
                    else go machines' messages' natMessage' lastNat'

        -- Nat message received
        go machines (message@[255, x, y]:messages) natMessage lastNat =
            go machines messages (Just message) lastNat

        -- Ordinary message delivered
        go machines (message@[to, x, y]:messages) natMessage lastNat =
            let machine = machines M.! to
                machine' = let state' = pushAll [x,y] machine in runUntilInput state'
                newMessages = chunksOf 3 . reverse . output $ machine'
                messages' = messages ++ newMessages
                machines' = M.insert to (clear machine') machines
            in go machines' messages' natMessage lastNat

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
    | length xs < n = error $ "Cannot divide list to chunks"
    | otherwise = take n xs : chunksOf n (drop n xs)

