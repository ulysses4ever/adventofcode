{-# language TypeSynonymInstances #-}
{-# language DerivingStrategies   #-}
{-# language PatternSynonyms      #-}
{-# language LambdaCase           #-}
{-# language FlexibleInstances    #-}

module AoC where

import Data.Char (isDigit)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.List (groupBy)


import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Flow ((.>)) --, (|>))
import Data.List.Extra (split, splitOn)

import Linear.V2
import Linear.V3

type P = V2 Int
pattern P a b = V2 a b

type P3 = V3 Int
pattern P3 a b c = V3 a b c

class Input a where
  input :: String -> a

type Str = BS.ByteString

instance Input Str where
  input = BSC.pack

groups :: [Char] -> [[Char]]
groups = splitOn "\n\n"

instance (Input a, Input b) => Input (a, b) where
  input = groups .> \case [a, b] -> (input a, input b)

instance (Input a, Input b, Input c) => Input (a, b, c) where
  input = groups .> \case [a, b, c] -> (input a, input b, input c)

instance Input a => Input [a] where
  input = lines .> map input

instance Input Value where
  input = BSC.pack .> BSC.fromStrict .> decode .> fromJust

instance Input Int where
  input = read

instance Input P where -- "x,y"
  input = split (== ',') .> \case [x,y] -> V2 (read x) (read y)

instance Input P3 where -- "x,y"
  input = split (== ',') .> \case [x,y,z] -> V3 (read x) (read y) (read z)

instance Input [Int] where
  input
    =  groupBy ((==) `on` isDigit)
    .> filter (head .> isDigit)
    .> map read
