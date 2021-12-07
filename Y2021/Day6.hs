
{-# language TupleSections #-}
module Y2021.Day6 (solve) where

import Aux

import Data.List
import Debug.Trace

import qualified Data.Map.Lazy as M
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)

type M = Map (Int, Int) Integer

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse n

type Rec = (Int,Int)

days :: Int -> Int
days 1 = 80
days 2 = 256
days _ = error "unknown part"

compute :: Int -> [Rec] -> Integer
compute _ rs = genericLength rs + sum (map (fromJust . (`M.lookup` m)) rs)

f :: Int -> Int -> Integer
f d t
  | firstTtl < 0 = 0
  | otherwise = z
  where
    firstTtl = t - d - 1
    cs = [x | t' <- takeWhile (>= 0) [firstTtl, firstTtl - 7..],
             let x = fromJust $ M.lookup (8,t') m ]
    z = genericLength cs + sum cs

m :: M
m = M.fromList [((d,t), f d t) | d <- [0..8], t <- [0..256] ]

parse :: Int -> String -> [Rec]
parse n = map (,days n) . readIntsSep ','
