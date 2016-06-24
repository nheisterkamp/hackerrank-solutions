module Main where

import System.IO (stdout, hFlush)
import Data.Char (chr, ord)

{-
 >   Increment data pointer so that it point to next location in memory.
 <   Decrement data pointer so that it point to previous locaion in memory.
 +   Increment the byte pointed by data pointer by 1. If it is already at its
     maximum value, 255, then new value will be 0.
 -   Decrement the byte pointed by data pointer by 1. If it is at its minimum
     value, 0, then new value will be 255.
 .   Output the character represented by the byte at the data pointer.
 ,   Read one byte and store it at the memory location pointed by data pointer.
 [   If the byte pointed by data pointer is zero, then move instruction pointer
     to next matching ']', otherwise move instruction pointer to next command.
 ]   If the byte pointed by data pointer is non-zero, then move instruction
     pointer to previous matching '[' command, otherwise to next command.
-}

maxOps = 100000

data BrainfuckCommand = GoRight      -- >
                      | GoLeft       -- <
                      | Increment    -- +
                      | Decrement    -- -
                      | Print        -- .
                      | Read         -- ,
                      | LoopL        -- [
                      | LoopR        -- ]
                      | Comment      -- anything else
                      deriving Show

type BrainfuckSource = [BrainfuckCommand]

parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = foldl foldCharToBF []
  where
    foldCharToBF r x = case (charToBF x) of 
      Comment -> r
      x' -> r ++ [x']
    charToBF '>' = GoRight
    charToBF '<' = GoLeft
    charToBF '+' = Increment
    charToBF '-' = Decrement
    charToBF '.' = Print
    charToBF ',' = Read
    charToBF '[' = LoopL
    charToBF ']' = LoopR
    charToBF  _  = Comment


data Tape a = Tape [a] -- Left of the pivot element
                    a  -- Pivot element
                   [a] -- Right of the pivot element
                   deriving Show

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
      where zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)

runBrainfuck :: String -> BrainfuckSource -> IO ()
runBrainfuck input = run input 0 emptyTape . bfSource2Tape
    where bfSource2Tape (b:bs) = Tape [] b bs

advance :: String                -- Input
        -> Int                   -- Operation
        -> Tape Int              -- Data tape
        -> Tape BrainfuckCommand -- Instruction tape
        -> IO ()

advance _ _ dataTape (Tape _ _ []) = return ()
advance input op dataTape source
  | op >= maxOps = putStrLn "\nPROCESS TIME OUT. KILLED!!!"
  | otherwise = run input op dataTape (moveRight source)

-- Interpret the command currently focused on the instruction tape
run :: String                -- Input
    -> Int                   -- Operation
    -> Tape Int              -- Data tape
    -> Tape BrainfuckCommand -- Instruction tape
    -> IO ()

run input op dataTape source@(Tape _ GoRight _) =
      advance input (op+1) (moveRight dataTape) source

run input op dataTape source@(Tape _ GoLeft  _) =
      advance input (op+1) (moveLeft dataTape) source

run input op (Tape l p r) source@(Tape _ Increment _) =
    advance input (op+1) (Tape l p' r) source
  where p' = if p == 255 then 0 else p+1

run input op (Tape l p r) source@(Tape _ Decrement _) =
    advance input (op+1) (Tape l p' r) source
  where p' = if p == 0 then 255 else p-1

run input op dataTape@(Tape _ p _) source@(Tape _ Print _) = do
    putChar (chr p)
    hFlush stdout
    advance input (op+1) dataTape source

run input op dataTape@(Tape l _ r) source@(Tape _ Read  _) = do
    advance (tail input) (op+1) (Tape l (ord (head input)) r) source

run input op dataTape@(Tape _ p _) source@(Tape _ LoopL  _)
    -- If the pivot is zero, jump to the
    -- corresponding LoopR instruction
    | p == 0 = seekLoopR input (op+1) 0 dataTape source
    -- Otherwise just ignore the `[` and continue
    | otherwise = advance input (op+1) dataTape source

run input op dataTape@(Tape _ p _) source@(Tape _ LoopR  _)
    | p /= 0 = seekLoopL input (op+1) 0 dataTape source
    | otherwise = advance input (op+1) dataTape source

run input op dataTape source@(Tape _ (Comment) _) = advance input op dataTape source

-- Move the instruction pointer left until a "[" is found.
-- The first parameter ("b" for balance) retains the current
-- bracket balance to find the matching partner. When b is 1,
-- then the found LoopR would reduce the counter to zero,
-- hence we break even and the search is successful.
seekLoopR :: String                -- Input
          -> Int                   -- Operation
          -> Int                   -- Parenthesis balance
          -> Tape Int              -- Data tape
          -> Tape BrainfuckCommand -- Instruction tape
          -> IO ()

seekLoopR input op 1 dataTape source@(Tape _ LoopR _) = advance input (op+1) dataTape source
seekLoopR input op b dataTape source@(Tape _ LoopR _) =
    seekLoopR input op (b-1) dataTape (moveRight source)
seekLoopR input op b dataTape source@(Tape _ LoopL _) =
    seekLoopR input op (b+1) dataTape (moveRight source)
seekLoopR input op b dataTape source =
    seekLoopR input op b dataTape (moveRight source)

seekLoopL :: String                -- Input
          -> Int                   -- Operation
          -> Int                   -- Parenthesis balance
          -> Tape Int              -- Data tape
          -> Tape BrainfuckCommand -- Instruction tape
          -> IO ()
seekLoopL input op 1 dataTape source@(Tape _ LoopL _) = advance input (op+1) dataTape source
seekLoopL input op b dataTape source@(Tape _ LoopL _) =
    seekLoopL input op (b-1) dataTape (moveLeft source)
seekLoopL input op b dataTape source@(Tape _ LoopR _) =
    seekLoopL input op (b+1) dataTape (moveLeft source)
seekLoopL input op b dataTape source =
    seekLoopL input op b dataTape (moveLeft source)


main :: IO ()
main = do
  -- raw <- getContents
  raw <- readFile "../data/BrainfuckInterpreter/input17.txt"
  let raw' = lines raw
      input = raw' !! 1
      code = (unlines . tail . tail) raw'

  runBrainfuck (init input) (parseBrainfuck code)
