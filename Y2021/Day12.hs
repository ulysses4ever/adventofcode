{-# language OverloadedStrings #-}
module Y2021.Day12 (solve) where

import Aux
import Data.List
import Data.List.Extra (nubOrd)
import Data.Word8
import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type V = B.ByteString
type Rec = (V,V)
type St = [Rec]
data St' = S [V] Bool

compute :: Int -> [Rec] -> Int
compute n g = trace (BC8.unpack $ B.intercalate "\n" res)
  (length res)
  where
    go :: [V] -> St' -> V -> [B.ByteString]
    go visited (S small s) v
      | v == "end" = [B.intercalate "," $ reverse (v:visited)]
      | v == "start" && not (null visited) = []
      | otherwise = concat (go (v:visited) <$> ss <*> as)
      where
        as = adj v g \\ small
        ss
          | not (isSmall v) = [S small s]
          | otherwise = sts
        sts
          | not s && n == 2 = [st', st'']
          | otherwise = [st'']
        st' = S small True
        st'' = S (v:small) s
    res = nubOrd $ go [] (S [] False) "start"

isSmall :: V -> Bool
isSmall = B.all isLower

adj :: V -> St -> [V]
adj v = concatMap pick
  where
    pick (v1,v2)
      | v == v1 = [v2]
      | v == v2 = [v1]
      | otherwise = []

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec s = (v1,v2)
  where
    [v1,v2] = map BC8.pack $ split (=='-') s
