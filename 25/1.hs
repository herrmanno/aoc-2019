import IntCode
import Data.Char (chr, ord)
import Data.List (intersperse)
import System.Environment (getArgs)

type Message = (Integer, Integer, Integer)

main = do
    args <- getArgs
    states <-
            case args of
                (file:_) -> do
                    states <- loadGame file
                    putStrLn $ "Loaded " ++ show (length (filter (not . halted) states)) ++ " valid states"
                    return states
                _ -> do
                    input <- readFile "input.txt"
                    let code = read $ "[" ++ input ++ "]"
                    return [initialState code []]
    go (head states) (tail states)
    where
        go :: State -> [State] -> IO ()
        go state history = do
            saveGame (state : history)
            let state' = runUntilInput state
            let out = reverse . output $ state'
            putStrLn (fmap (chr . fromIntegral) out)
            input <- getLine
            if input == "back"
                then go (head history) (tail history)
                else do
                    let state'' = push 10 . pushAll (fmap (fromIntegral . ord) input) $ state'
                    go (clear state'') (state : history)
        loadGame path =
            read <$> readFile path
        saveGame states = do
            let text = show states
            writeFile "save.txt" text


{-
- klein bottle
- loom
- mutex
- pointer
- polygon
- hypercube
- mug
- manifold

lighter than allowed
- klein bottle
- mutex
- hypercube
- polygon
- mug

heavier than allowed:
- manifold
- loom
- pointer
-}

