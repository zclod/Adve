module Main where

import           Prelude hiding (filter, interact, length)
import           Control.Comonad
import           Control.Comonad.Store
import           Data.Sequence (Seq, length, index, fromList)
import           Data.Text (Text, unpack, filter, pack)
import           Data.Text.IO (interact)
import           Data.Char

type CircularList a = Store Int a

mkCircular :: Seq a -> CircularList a
mkCircular xs = store circularAccess 0
    where
        circularAccess i = index xs (i `mod` size)
        size = length xs

findNonZero :: (CircularList Int -> CircularList Int) -> CircularList Int -> Int
findNonZero movement w
  | extract w /= (extract . movement) w = 0
  | otherwise                           = extract w

moveRightOf :: Int -> CircularList Int -> CircularList Int
moveRightOf offset w = store f newPos
    where
        (f, i) = runStore w
        newPos = i + offset

easyMove :: CircularList Int -> CircularList Int
easyMove = moveRightOf 1

hardMove :: Int -> CircularList Int -> CircularList Int
hardMove size = moveRightOf (size `quot` 2)

mkSolution :: (CircularList Int -> CircularList Int) -> Seq Int -> Int
mkSolution movement input = sum solution
    where
        solution = fmap (`peek` nonZero) [0..(size - 1)]
        nonZero = extend (findNonZero movement) clist
        clist = mkCircular input
        size = length input



easySolution :: Seq Int -> Int
easySolution = mkSolution easyMove
hardSolution :: Seq Int -> Int
hardSolution input = mkSolution (hardMove $ length input) input

parseInput :: Text -> Seq Int
parseInput = fmap digitToInt . fromList . unpack . filter isDigit

main :: IO ()
main = interact $ pack . show . hardSolution . parseInput
