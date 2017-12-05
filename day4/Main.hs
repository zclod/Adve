module Main where

import           Prelude (IO, Bool(..), (.), (*>), (==), (||), ($), show, otherwise, fmap, putStrLn, foldl, id, length, filter)
import           Data.List (sort)
import           Data.Attoparsec.Text
import           Data.Text (Text, unlines, lines, pack, unpack)
import           Data.Text.IO (interact)
import           Data.Set(Set(..), empty, member, insert)
import           Data.Char (isAlphaNum)
import           Data.Traversable

type Line = [Text]

data S = S { _words :: Set Text
           , _valid :: Bool
           }

zero = S empty True

step (S ws v) w
  | w `member` ws = S ws False
  | otherwise    = S (w `insert` ws) v

sortWord :: Text -> Text
sortWord = pack . sort . unpack

stepHard (S ws v) w
  | sorted `member` ws = S ws False
  | otherwise    = S (sorted `insert` ws) v
    where
        sorted = sortWord w

passValid :: Line -> Bool
passValid = _valid . foldl step zero

passValidHard :: Line -> Bool
passValidHard = _valid . foldl stepHard zero

passCount = length . filter id


pWord = skipSpace *> takeWhile1 isAlphaNum

pLine :: Parser Line
pLine = many' pWord

parseInput = sequenceA . fmap (parseOnly pLine) . lines

main :: IO ()
main = interact $ pack . show . fmap (passCount . fmap passValidHard ) . parseInput
