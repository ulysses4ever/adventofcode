{-# language BangPatterns #-}
module Y2021.Day7 (solve) where

import Aux
import Relude.Extra.Foldable (average)
import Data.List
import Data.Maybe
import Debug.Trace

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type Rec = Int

compute :: Int -> [Rec] -> Int
compute n = sumsToMidpoint n <*> findMidpoint n

findMidpoint :: Int -> [Rec] -> Int
findMidpoint 1 rs = sort rs !! (length rs `div` 2) -- median

-- don't ask why `floor` instead of `round`, don't get me started on the banker's rounding method...
findMidpoint 2 rs = floor . fromJust . average . map fromIntegral $ rs -- average

findMidpoint _ _ = error "unknown part"

sumsToMidpoint :: Int -> [Rec] -> Int -> Int
sumsToMidpoint 1 rs mid = sum $ map (abs . (+ negate mid)) rs
sumsToMidpoint 2 rs mid = sum $ map (ap . abs . (+ negate mid)) rs
sumsToMidpoint _ _ _ = error "unknown part"

ap n = n * (n + 1) `div` 2

parse :: String -> [Rec]
parse = readIntsSep ','
