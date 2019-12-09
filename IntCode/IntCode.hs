module IntCode (State(..),run,runWhile,initialState) where

import qualified Data.Map as M
import qualified System.Environment as E
import Data.Maybe (fromMaybe)

---------------------------
--        types
---------------------------

data State = State
  { ip :: Integer
  , relp :: Integer
  , mem :: Memory
  , input :: [Integer]
  , output :: [Integer]
  , halted :: Bool
  , lastInstr :: Maybe Instruction
  } deriving (Show)

initialState :: [Integer] -> [Integer] -> State
initialState code inputs = State
  { ip = 0
  , relp = 0
  , mem = M.fromList $ zip [0..] code
  , input = inputs
  , output = []
  , halted = False
  , lastInstr = Nothing
  }

type Memory = M.Map Integer Integer

readM :: Integer -> Memory -> Integer
readM i m = fromMaybe 0 $ M.lookup i m

writeM :: Integer -> Integer -> Memory -> Memory
writeM = M.insert

data Instruction = Add | Mult | Input | Output | JmpNZ | JmpZ | Lt | Eq | IncRelp | Halt | Other Integer deriving (Show, Eq)

data ParameterMode = Ref | Value | Rel deriving (Eq)

pmodeFromInteger :: Integer -> ParameterMode
pmodeFromInteger 2 = Rel
pmodeFromInteger 1 = Value
pmodeFromInteger _ = Ref

---------------------------
--      functions
---------------------------

run :: State -> State
run s = let
  s' = step s
  in if (halted s') then s' else run s'

runWhile :: (State -> Bool) -> State -> State
runWhile p s
  | p s = let
    s' = step s
    in if (halted s') then s' else run s'
  | otherwise = s

runD :: State -> IO State
runD s = do 
    print s { mem = M.empty }
    print $ take 4 $ drop (fromIntegral (ip s)) $ M.elems (mem s)
    s' <- stepD s
    if (halted s') then return s' else runD s'

step :: State -> State
step s = let
  (instr, args) = parseState s
  in applyInstr instr args s

stepD :: State -> IO State
stepD s = do
  let (instr, args) = parseState s
  print (instr, args)
  return $ applyInstr instr args s

applyInstr :: Instruction -> [Integer] -> State -> State
applyInstr Add (a1:a2:a3:_) s   = s { ip = (ip s) + 4, mem = writeM a3 (a1 + a2) (mem s) }
applyInstr Mult (a1:a2:a3:_) s  = s { ip = (ip s) + 4, mem = writeM a3 (a1 * a2) (mem s) }
applyInstr Input (a1:_) s       = s { ip = (ip s) + 2, mem = writeM a1 (head (input s)) (mem s), input = tail (input s) }
applyInstr Output (a1:_) s      = s { ip = (ip s) + 2, output = a1 : (output s) }
applyInstr JmpNZ (a1:a2:_) s
  | a1 /= 0                     = s { ip = a2 }
  | otherwise                   = s { ip = (ip s) + 3 }
applyInstr JmpZ (a1:a2:_) s
  | a1 == 0                     = s { ip = a2 }
  | otherwise                   = s { ip = (ip s) + 3 }
applyInstr Lt (a1:a2:a3:_) s
  | a1 < a2                     = s { ip = (ip s) + 4, mem = writeM a3 1 (mem s) }
  | otherwise                   = s { ip = (ip s) + 4, mem = writeM a3 0 (mem s) }
applyInstr Eq (a1:a2:a3:_) s
  | a1 == a2                    = s { ip = (ip s) + 4, mem = writeM a3 1 (mem s) }
  | otherwise                   = s { ip = (ip s) + 4, mem = writeM a3 0 (mem s) }
applyInstr IncRelp (a1:_) s     = s { ip = (ip s) + 2, relp = (relp s) + a1 }
applyInstr Halt _ s             = s { halted = True }
applyInstr instr _ s            = s { ip = (ip s) + 1, halted = True, lastInstr = Just instr }

parseState :: State -> (Instruction, [Integer])
parseState s = let
    instr = parseInstr s
    args = parseArgs instr s
    in (instr, args)

parseInstr :: State -> Instruction
parseInstr s = let
    i = readM (ip s) (mem s) `mod` 100
    in case i of
      1 -> Add
      2 -> Mult
      3 -> Input
      4 -> Output
      5 -> JmpNZ
      6 -> JmpZ
      7 -> Lt
      8 -> Eq
      9 -> IncRelp
      99 -> Halt
      _ -> Other i
    
parseArgs :: Instruction -> State -> [Integer]
parseArgs instr s = parseTypedArg <$> zip3 (repeat instr) (parameterTypes instr s) [1..]
      where
        parameterTypes _ s = let
            n = readM (ip s) (mem s)
            ns = (\i -> n `mod` (10 ^ (i + 1)) `div` (10 ^ i)) <$> ([2..4] :: [Integer])
            in map pmodeFromInteger ns
        parseTypedArg (Input,Rel,idx) = readTargetRel idx
        parseTypedArg (Input,_,idx) = readValue idx
        parseTypedArg (_,Rel,idx@3) = readTargetRel idx
        parseTypedArg (_,_,idx@3) = readValue idx
        parseTypedArg (_,Rel,idx) = readRel idx
        parseTypedArg (_,Value,idx) = readValue idx
        parseTypedArg (_,_,idx) = readRef idx
          where
        readRef idx       = readM (readM ((ip s) + idx) (mem s)) (mem s)
        readValue idx     = readM ((ip s) + idx) (mem s)
        readRel idx       = readM ((relp s) + readM ((ip s) + idx) (mem s)) (mem s)
        readTargetRel idx = (relp s) + readM ((ip s) + idx) (mem s)

        
---------------------------
--        main
---------------------------

main :: IO ()
main = do
  (debug,(file:args)) <- isDebug <$> E.getArgs
  content <- readFile file
  let code = read $ "[" ++ content ++ "]"
  let s = initialState code (read <$> args)
  
  s' <- if debug then runD s else return $ run s
  print (output s')
  where
    isDebug xs = let
      xs' = filter (/="-d") xs
      in (length xs /= length xs', xs')
