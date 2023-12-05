module Input where

import Data.Char (isDigit)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.List (groupBy)
import Debug.Trace
import Data.Coerce

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Flow ((.>), (|>))
import Data.List.Extra (split, splitOn)
import Text.Pretty.Simple (pPrint)

import Types

class Input a where
  input :: String -> a

groups :: String -> [String]
groups = splitOn "\n\n"

newtype Groups a = Gs [a]
  deriving Show

newtype Lines a = Ls [a]
  deriving Show

instance (Input a, Input b) => Input (a, b) where
  input = groups .> \case [a, b] -> (input a, input b)

instance (Input a, Input b, Input c) => Input (a, b, c) where
  input = groups .> \case [a, b, c] -> (input a, input b, input c)

instance Input Value where -- AoC tasks often use JSON as an input
  input = BSC.pack .> BSC.fromStrict .> decode .> fromJust

instance Input Int where
  input = read

instance Input P where -- "x,y"
  input = split (== ',') .> \case [x,y] -> P (read x) (read y)

instance Input P3 where -- "x,y,z"
  input = split (== ',') .> \case [x,y,z] -> P3 (read x) (read y) (read z)

instance {-# OVERLAPPING  #-} Input [Int] where
  input
    =  groupBy ((==) `on` isDigit)
    .> filter (head .> isDigit)
    .> map read

instance {-# OVERLAPPING  #-} Input String where
  input = id

instance {-# OVERLAPPABLE #-} Input a => Input [a] where
  input = lines .> map input

instance Input a => Input (Groups a) where
  input = groups .> map input .> Gs

instance Input a => Input (Lines a) where
  input = lines .> map input .> Ls
