module Aux where

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I
import qualified Data.List.Extra as E (split)
import Data.List
import Data.Int
import Control.Arrow (first, second)

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

-- Working with matricies presented as Map (Int, Int) a
showMat :: (Integral i, Integral j, Show a) => String -> j -> j -> M.Map (i, i) a -> String
showMat sep rw cl m = unlines rows
  where
    rows = map showRow $ nats01 (fromIntegral rw)
    showRow r = intercalate sep $ map (\c -> show $ m M.! (r,c)) $ nats01 (fromIntegral cl)

-- IMat is any matrix represented as IntMap a.
readIMat :: (Char -> Bool) -> (Char -> a) -> String -> (I.IntMap a, Int, Int)
readIMat chk conv s = (m, r, c)
  where
    ls = lines s
    r = length ls
    c = length $ head ls
    cnts = concat ls
    m = I.fromDistinctAscList
      . map (second conv)
      . filter (chk . snd)
      $ zip (nats01 $ length s) cnts

showIMat :: Show a => String -> String -> (I.IntMap a, Int, Int) -> String
showIMat sep def (m, rw, cl) = unlines rows
  where
    rows = map showRow $ nats01 rw
    showRow r = intercalate sep $ map (\c -> showElem (r*cl + c)) $ nats01 cl
    showElem n = case m I.!? n of
      Nothing -> def
      Just e  -> show e
