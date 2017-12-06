module Main where

import           Prelude hiding (interact, unlines)
import           Data.Attoparsec.Text
import           Data.Text (Text, unlines, lines, pack, unpack)
import           Data.Text.IO (interact)
import           Data.List.Zipper

type Maze = Zipper Int
type Count = Int

moveOf :: Int -> Maze -> Maze
moveOf n maze
    | n == 0 = maze
    | n >  0 = moveOf (n-1) (right maze)
    | n <  0 = moveOf (n+1) (left maze)

incrementOf :: Int -> Maze -> Maze
incrementOf n maze = replace (n + cursor maze) maze

eval :: Count -> Maze -> Count
eval count maze
    | safeCursor maze == Nothing = count
    | otherwise                  = eval (count + 1) maze'
        where
            maze' = moveOf (cursor maze) (incrementOf 1 maze)

evalHard :: Count -> Maze -> Count
evalHard count maze
    | safeCursor maze == Nothing = count
    | otherwise                  = evalHard (count + 1) maze'
        where
            maze' = moveOf offset (incrementOf inc maze)
            offset = cursor maze
            inc = if offset >= 3 then (-1) else 1

pInput :: Parser [Int]
pInput = many' $ skipSpace *> signed decimal

parseInput = parseOnly pInput

{-

:set -XOverloadedStrings
:r
maze = fromList [0,3,0,1,-3]
maze = fromList [2,3,2,3,-1]
evalHard 0 maze
eval 0 maze

parseInput $ unlines ["0","3","0","1","-3"]

eval 0 maze

-}

main :: IO ()
main = interact $ pack . show . fmap (evalHard 0 . fromList) . parseInput
-- main = putStrLn "day 5"
