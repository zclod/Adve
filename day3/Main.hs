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


-- il numero massimo al circolo n e x^2 dove x = (2*n+1)^2 (dispari) 1,3,5
-- il numero minimo al circolo n e (2*(n-1)+1)^2 + 1
-- funzione ordinamento 
--      1 quadrante \ring (x,y) -> y + (x - ring)


main :: IO ()
--main = interact $ pack . show . hardSol . parseInput
main = print "day 3"

