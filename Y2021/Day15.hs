{-# language BangPatterns  #-}
{-# language TupleSections #-}
module Y2021.Day15 (solve) where

import Aux
import Data.List
import Debug.Trace
import qualified Data.Vector.Primitive as V
import qualified Data.IntPSQ as Q


-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Dist = Int
type Idx = Int
type M = (V.Vector Dist, Int)
type Q = Q.IntPSQ Dist Idx
data St = S

compute :: Int -> M -> Int
compute n (v,cl) = res
  where
    target = V.length v
    key (k,_,_) = k
    prio (_,p,_) = p
    q :: Q
    q = Q.fromList ((,maxBound, -1) <$> [0..V.length v])
    go :: Q -> Int
    go q
      | key $ Q.findMin q == target = prio $ Q.findMin q
      | otherwise = go $ step q
    res = 0

    step :: Q -> Q
    step q = q''
      where
        Just (v, d, p, q') = Q.minView q
        ns = filter (Q.member q') $ nhood v
        q'' = foldl' (updFrom p) q' ns
    updFrom :: Dist -> Q -> Idx -> Q
    updFrom toCur q v
      | toCur + dist v < fst . Q.lookup v $ q = undefined

parse :: String -> M
parse i = (v,cl)
  where
    ls = lines i
    cl = length (head ls)
    v  = V.fromList (map (read . pure) $ concat ls)

readRec :: String -> M
readRec = undefined
