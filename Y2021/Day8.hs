{-# language BangPatterns #-}
module Y2021.Day8 (solve) where

import Aux
import Data.List
import Debug.Trace
import Control.Arrow (second)

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

data Rec = R {
  left :: [String],
  right :: [String] }
  deriving Show
data St = S

interestingLengths :: [Int]
--                    1 7 4 8
interestingLengths = [2,3,4,7]

compute :: Int -> [Rec] -> Int
compute 1 rs = res
  where
  countInteresting :: [String] -> Int
  countInteresting = length . filter ((`elem` interestingLengths ) . length)
  res = sum $ map (countInteresting . right) rs

compute 2 rs = res
  where
  res = 0

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec = uncurry R . second tail . break (== "|") . words
