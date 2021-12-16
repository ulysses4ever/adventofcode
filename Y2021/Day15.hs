{-# language BangPatterns  #-}
{-# language TupleSections #-}
module Y2021.Day15 (solve) where

import Aux
import Data.List
import Data.Maybe
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
compute n (m,cl) = res
  where
    max 1 = V.length m
    max 2 = 25 * V.length m
    mx = max n
    cols 1 = cl
    cols 2 = 5 * cl
    cl' = cols n
    target = mx - 1
    key (k,_,_) = k
    prio (_,p,_) = p

    q :: Q
    q = Q.insert 0 0 (-1) $
      Q.fromList ((,maxBound, -1) <$> [1..mx - 1])

    go :: Q -> Int
    go q
      | (key . findMin $ q) == target
          = prio $ findMin q
      | otherwise = go $ step q
    res = go q

    step :: Q -> Q
    step q = -- trace ("open: " ++ show v ++ " whith dist: " ++ show d)
      q''
      where
        Just (v, d, prev, q') = Q.minView q
        ns = filter (`Q.member` q') $ nhood' v
        q'' = foldl' (updFrom v d) q' ns

    updFrom :: Idx -> Dist -> Q -> Idx -> Q
    updFrom cur toCur q v
      | alt < (fst . lookupQ v $ q)
          = -- trace (unwords ["updFrom: alt is", show alt, ", cur is", show cur, ", to is", show v]) $
            Q.insert v alt cur q
      | otherwise = q
      where
        alt = toCur + d v

    dist :: Int -> Idx -> Dist
    dist 1 v = m V.! v
    dist 2 v = n
      where
        r = v `div` cl'
        c = (v `mod` cl') `div` cl
        base = m V.! (v `mod` cl)
        n = (base - 1 + c + r) `div` 8

    d = dist n
    nhood' = nhood mx cl'

nhood max cl u
  | u `mod` cl == 0 = filter ((/= cl - 1) . (`mod` cl)) full
  | u `mod` cl == cl - 1 = filter ((/= 0) . (`mod` cl)) full
  | otherwise = full
  where
    full = [u' | du <- [-1, 1, negate cl, cl],
            let u' = u + du,
            u' >= 0,
            u' < max]

findMin :: Q -> (Idx, Dist, Idx)
findMin = fromJust . Q.findMin

lookupQ :: Idx -> Q -> (Dist, Idx)
lookupQ v = fromJust . Q.lookup v

parse :: String -> M
parse i = -- trace (unwords ["parsed matrix with ", show cl, " columns and ", show $ V.length v, "elements; contents: ", show $ V.toList v])
  (v,cl)
  where
    ls = lines i
    cl = length (head ls)
    v  = V.fromList (map (read . pure) $ concat ls)

readRec :: String -> M
readRec = undefined
