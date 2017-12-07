{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified Prelude as P
import           Prelude hiding (unlines, filter, interact, length, lines, min, max)
import           Data.Monoid
import           Data.Sequence (Seq, length, index, fromList)
import           Data.Text (Text, unpack, unlines, lines, filter, pack)
import           Data.Text.IO (interact)
import           Text.Read(readMaybe)
import           Data.Attoparsec.Text

orderedsquare n = s1 <> s2 <> s3 <> s4
  where
    s1 = fromList $ P.zip (repeat 0) [0..(n-2)]
    s2 = fromList $ P.zip [-1,-2..(-n+1)] (repeat (n-2))
    s3 = fromList $ P.zip (repeat (-n+1)) [(n-3),n-4..(-1)]
    s4 = fromList $ P.zip [(-n+2),-n+3..0] (repeat (-1))

translate (dx,dy) (x,y) = (x+dx, y+dy)

findBasePoint n = (n, n-1)

-- distanceFromBase n = 

precSquare n = go 1
  where
    go x
      | (2*x+1) *  (2*x+1) > n = x-1
      | otherwise = go (x + 1)

{-

:r
fmap (translate (2,-1)) (orderedsquare 5)

findBasePoint 1

precSquare 9

:t repeat
:t P.zip

-}

main :: IO ()
--main = interact $ pack . show . hardSol . parseInput
main = print "day 3"

