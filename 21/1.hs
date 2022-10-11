import IntCode
import Data.Char (ord, chr)
import Text.Printf (printf)


main = do
    input <- readFile "input.txt"
    let code = read $ "[" ++ input ++ "]"
    go 1 code instructions1
    go 2 code instructions2
    where
        go :: Int -> [Integer] -> [String] -> IO ()
        go part code instructions = do
            let input = fromIntegral . ord <$> unlines instructions
            let state = run $ initialState code input
            let out = reverse $ output state
            if any (>255) out
                then printf "Part %d: %d\n" part (last out)
                else putStrLn $ fmap (chr . fromIntegral) out

        {- Jump if there is an obstacle in the next three fields, but the fourth field (where we
            are landing) is floor.

            This would only fail if we jump to early and land in a place where we neither can
            jump again nor can we walk another step:

            @---v
            # ### ## #
            ABCD......
            ----------
            0000000001
            1234567890

            Fortunately, this does not occur at part 1.
        -}
        instructions1 =
            [
            "OR A J",
            "AND B J",
            "AND C J",
            "NOT J J",
            "AND D J",
            "WALK\n"
            ]

        {- Jump if there is an obstacle in the next three fields, but the fourth field (where we
            are landing) is floor *and we can go on from there*.

            In part 2 there seem to occur cases, where one fails if jumping to early.
            Thoses cases are guarded by the additional condition `E v H`, that guarantees that
            we can take another step or jump again after landing.

            @---v
            # ### ## #
            ABCDEFGHI
            ----------
            0000000001
            1234567890

            Fortunately, this does not occur at part 1.
        -}
        instructions2 =
            [
            "OR A J",
            "AND B J",
            "AND C J",
            "NOT J J",
            "AND D J",

            "OR E T",
            "OR H T",
            "AND T J",

            "RUN\n"
            ]