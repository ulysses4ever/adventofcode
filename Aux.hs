module Aux where

import qualified Data.Map.Strict as M
import qualified Data.List.Extra as E (split)
import Data.List
import Data.Int

readIntsSep :: Char -> String -> [Int]
readIntsSep c = map read . split (== c)

sign :: Int -> Int
sign = signum

split :: (a -> Bool) -> [a] -> [[a]]
split = E.split

nats01 :: Integral i => i -> [i]
nats01 n = [0..n-1]
nats02 :: Integral i => i -> i -> [(i,i)]
nats02 n m = [(i,j) | i <- nats01 n, j <- nats01 m]
{-# INLINEABLE nats01 #-}
{-# INLINEABLE nats02 #-}
{-# SPECIALIZE nats01 :: Int -> [Int] #-}
{-# SPECIALIZE nats01 :: Int8 -> [Int8] #-}
{-# SPECIALIZE nats02 :: Int -> Int -> [(Int,Int)] #-}
{-# SPECIALIZE nats02 :: Int8 -> Int8 -> [(Int8,Int8)] #-}

-- Working with matricies presented as Mat (Int, Int) a
showMat :: (Integral i, Integral j, Show a) => String -> j -> j -> M.Map (i, i) a -> String
showMat sep rw cl m = unlines rows
  where
    rows = map showRow $ nats01 (fromIntegral rw)
    showRow r = intercalate sep $ map (\c -> show $ m M.! (r,c)) $ nats01 (fromIntegral cl)
