{-# language BangPatterns  #-}
{-# language TupleSections #-}
module Y2021.Day6 (solve) where

import Aux

import Data.List
import Debug.Trace

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Rec = (Int,Int)
data St = S {
  total :: !Int,
  recs  :: [Rec] }

days :: Int
days = 80

compute :: Int -> [Rec] -> Int
compute 1 rs = res
  where
  initSt = S 0 rs
  res = total . head . dropWhile (\s -> not $ null $ recs s) . iterate update $ initSt

compute 2 rs = res
  where
  res = 0

update :: St -> St
update (S n (r:rs)) = S (n+1) (rs ++ rs')
  where
  (d, ttl) = r
  firstTtl = ttl - d
  ttls = takeWhile (> 0) [firstTtl, firstTtl - 6..]
  rs' = zip [8,8..] ttls


parse :: String -> [Rec]
parse = map (,days) . readIntsSep ','
