{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified Prelude as P
import           Prelude hiding (unlines, filter, interact, length, lines, min, max)
import           Data.Sequence (Seq, length, index, fromList)
import           Data.Text (Text, unpack, unlines, lines, filter, pack)
import           Data.Text.IO (interact)
import           Text.Read(readMaybe)
import           Data.Char
-- import           GHC.Generics
import           Data.Attoparsec.Text
-- import           Control.Newtype
import           Data.Semigroup
import           Data.Fold.M1
import           Control.Monad.Zip
import           Control.Arrow
import           Data.Profunctor
import qualified Data.List.NonEmpty as NE

-- instance Newtype (Max a) a where
--     pack = Max
--     unpack (Max a) = a

-- instance Newtype (Min a) a where
--     pack = Min
--     unpack (Min a) = a


type Cell = Int
type Spreadshit = [[Cell]]

pCell :: Parser Cell
pCell = do
    skipSpace
    decimal

pRow :: Parser [Cell]
pRow = many' pCell

parseInput :: Text -> Spreadshit
parseInput = fmap (either (const []) id . parseOnly pRow) . lines

-------------------------------------------------
    -- easy solution

max :: M1 Int Int
max = M1 getMax Max (<>)

min :: M1 Int Int
min = M1 getMin Min (<>)

maxmin :: M1 Int (Int, Int)
maxmin = mzip max min

lineCS :: M1 Int Int
lineCS = rmap (uncurry (-)) maxmin

easySol :: [[Int]] -> Int
easySol xxs = sum $ fmap (flip runM1 lineCS . NE.fromList) xxs
-------------------------------------------------
    --hard solution

hardLineCS :: [Int] -> Int
hardLineCS xs = sum $ mydivide <$> xs <*> xs
    where
        mydivide a b 
          | a `mod` b == 0 && a /= b = a `quot` b
          | otherwise                = 0

hardSol :: [[Int]] -> Int
hardSol = sum . fmap hardLineCS
-------------------------------------------------

main :: IO ()
main = interact $ pack . show . hardSol . parseInput
-- main = print "day 2"

