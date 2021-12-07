
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
  recs  :: [[Rec]] }
  deriving Show

days :: Int
days = 256

compute :: Int -> [Rec] -> Int
compute 1 rs = res
  where
  initSt = S 0 [rs]
  res = total . head . dropWhile (not . null . recs) . iterate update $ initSt

compute 2 rs = res
  where
  res = 0

update :: St -> St
update (S n ((r:rs):rss)) =  -- traceShow res
  res
  where
  res = S (n+1) (rs : rs' : rss)
  (d, ttl) = r
  firstTtl = ttl - d - 1
  ttls = takeWhile (>= 0) [firstTtl, firstTtl - 7..]
  rs' = zip [8,8..] ttls
update s@(S n ([]:rss)) = -- traceShow s $
  update $ S n rss
update s = s

parse :: String -> [Rec]
parse = map (,days) . readIntsSep ','
