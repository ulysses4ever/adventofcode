#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers
-}
{-# language TypeApplications, RecordWildCards, StrictData, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Numeric
import Data.Bits
import Data.List
import qualified Data.IntMap.Strict as M

type Str = String
type MaskC = Int -> Int
type M = M.IntMap Int

-- Part 1

data S = S {
  mask :: MaskC,
  mem :: M }

initS :: S
initS = S id M.empty

compileMask :: Str -> MaskC
compileMask xs = foldl' f id xis
  where
  xis = filter ((/= 'X') . snd) . zip [0..] . reverse $ xs
  f m (i, c) = m . case c of
    '0' -> flip clearBit i
    '1' -> flip setBit   i
    _   -> error "compileMask: invalid input"

procLine :: S -> Str -> S
procLine s@S{..} (words -> ws)
  | "mask" == ws !! 0 = s {mask = compileMask $ ws !! 2}
  | otherwise         = s {mem = M.insert k y mem}
  where
  y = mask (read @Int $ ws !! 2)
  k = fst . head . readDec . tail . dropWhile (/= '[') $ ws !! 0

-- Part 2

type MaskC2 = Int -> [Int]

data S2 = S2 {
  mask2 :: MaskC2,
  mem2 :: M }

initS2 :: S2
initS2 = S2 (pure . id) M.empty

compileMask2 :: Str -> MaskC2
compileMask2 xs = foldl' f id xis . pure
  where
  xis = zip [0..] . reverse $ xs
  f m (i, c) = m . case c of
    '0' -> fmap id
    '1' -> fmap $ flip setBit i
    'X' -> concatMap (\y -> [setBit y i, clearBit y i])
    _   -> error "compileMask2: invalid input"

procLine2 :: S2 -> Str -> S2
procLine2 s@S2{..} (words -> ws)
  | "mask" == ws !! 0 = s {mask2 = compileMask2 $ ws !! 2}
  | otherwise         = s {mem2 = foldl' (\mem' a' -> M.insert a' x mem') mem2 (mask2 a) }
  where
  x = read @Int $ ws !! 2
  a = fst . head . readDec . tail . dropWhile (/= '[') $ ws !! 0


-- Main

getInput :: IO [String]
getInput =
  lines <$> readFile "input/day-14.txt"

main :: IO ()
main = do
  inp <- getInput
  let sumMap = M.foldr' (+) 0
      res1 = sumMap . mem . foldl' procLine initS $ inp
  print res1
  let res2 = sumMap . mem2 . foldl' procLine2 initS2 $ inp
  print res2
  print "--"
