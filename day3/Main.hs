{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified Prelude as P
import           Prelude hiding (unlines, filter, take, interact, length, lines, min)
import           Data.Monoid
import           Data.Sequence (Seq, length, index, fromList, filter, take, singleton, elemIndexL)
import           Data.Text (Text, unpack, unlines, lines, pack)
import           Data.Text.IO (interact)
import           Text.Read(readMaybe)
import           Data.Attoparsec.Text hiding (take)
import           Data.Function.Memoize

orderedsquare 0 = singleton (0,0)
orderedsquare ring = s1 <> s2 <> s3 <> s4
  where
    s1 = fromList $ P.zip (repeat 0) [0..(n-2)]
    s2 = fromList $ P.zip [-1,-2..(-n+1)] (repeat (n-2))
    s3 = fromList $ P.zip (repeat (-n+1)) [(n-3),n-4..(-1)]
    s4 = fromList $ P.zip [(-n+2),-n+3..0] (repeat (-1))
    n = 2*ring+1

translate (dx,dy) (x,y) = (x+dx, y+dy)

findBasePointCoord 0 = (0,0)
findBasePointCoord n = (n, -(n-1))

maxRingNumber n = (2*n+1) *  (2*n+1)
minRingNumber n = 1 + maxRingNumber (n-1)

precSquare n = go 0
  where
    go x
      | maxRingNumber x >= n = ((maxRingNumber (x-1) + 1), x)
      | otherwise = go (x + 1)

findNumber' 0 = ((0,0),0,0)
findNumber' n = ((translate base coord), ring, delta)
    where
        (basesquare, ring) = precSquare n
        delta = n - basesquare
        base = findBasePointCoord ring
        coord = (orderedsquare ring) `index` delta

reverseIndex :: (Int,Int) -> Int
reverseIndex (0,0) = 1
reverseIndex p@(x,y) = delta + minRingNumber ring
    where
        ring = max (abs x) (abs y)
        delta = maybe 0 id (elemIndexL p ixs)
        ixs = fmap (translate base) $ orderedsquare ring
        base = findBasePointCoord ring

findNumber 1 = (0,0)
findNumber n = translate base coord
    where
        (basesquare, ring) = precSquare n
        delta = n - basesquare
        base = findBasePointCoord ring
        coord = (orderedsquare ring) `index` delta

manhattanFrom :: (Int,Int) -> Int -> Int
manhattanFrom (x0,y0) n = abs (x-x0) + abs (y-y0)
    where
        (x,y) = findNumber n

isNeighbour :: (Int,Int) -> (Int,Int) -> Bool
isNeighbour (x,y) (nx,ny) = (floor . sqrt . fromIntegral) (square (x-nx) + square (y-ny)) == 1
    where
        square n = n*n

neighbours ix = filter (isNeighbour coord) candidates
    where
        candidates = (fmap (translate $ findBasePointCoord ring) (take (delta+1) $ orderedsquare ring)) <> (fmap (translate $ findBasePointCoord (ring-1))) (orderedsquare (ring-1))
        (coord, ring, delta) = findNumber' ix

neighbours' = neighbours . reverseIndex




neighboursFast = memoize neighbours
-- accessorHard :: Int -> Int
accessorHard 1 = 1
accessorHard ix = sum $ fmap (accessorHardFast . reverseIndex) $ neighbours ix

accessorHardFast = memoize accessorHard


solutionHard n target
  | current >= target = current
  | otherwise         = solutionHard (n+1) target
    where
        current = accessorHardFast n

-- solution easy
-- manhattanFrom (0,0) puzzleInput
{-

:r

solutionHard 0 95011
solutionHard 0 95
p = findNumber' 3
print p
neighbours' (1,1)
minRingNumber 2
reverseIndex (0,0)
accessorHard 50
findNumber 2
neighbours 9
fmap reverseIndex $ neighboursFast 10
fmap reverseIndex $ neighbours 10
solutionHard 0 347991
-}

main :: IO ()
--main = interact $ pack . show . hardSol . parseInput
main = print "day 3"

