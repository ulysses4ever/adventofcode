#!/usr/bin/env cabal
{- cabal:
build-depends: base, extra, unordered-containers
-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Data.List.Extra
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

type Str = String
type Map = M.HashMap
type Label = Str
type BagReq = (Label, Int)

readReq :: [Str] -> BagReq
readReq (count : n1 : n2 : _) = (unwords [n1, n2], read count)
readReq _ = error "readReq"

-- Part 1: Up
type BagDB = Map Label [Label] -- inner -> outers

readBag :: Str -> [(Label, [Label])]
readBag s = zip reqs (repeat [name])
  where
  n1 : n2 : "bags"  : "contain" :  rest = words s
  name = unwords [n1, n2]
  reqs = if length rest == 3 -- "no other bags"
    then []
    else map (fst . readReq) (chunksOf 4 rest)

allPaths :: Label -> BagDB -> Int
allPaths q db = (length $ go S.empty [q]) - 1
  where
  go black (g:grey)
    | g `S.member` black = go black grey
    | otherwise = let
          grey' = grey ++ maybe [] id (db M.!? g)
        in go (g `S.insert` black) grey'
  go black [] = black

-- Part 2
type Bag = (Label, [BagReq])
type BagDB2 = Map Label [BagReq] -- outer -> inners

readBag2 :: Str -> Bag
readBag2 s = (name, reqs)
  where
  n1 : n2 : "bags"  : "contain" :  rest = words s
  name = unwords [n1, n2]
  reqs = if length rest == 3 -- "no other bags"
    then []
    else map (readReq) (chunksOf 4 rest)

allPaths2 :: Label -> BagDB2 -> Int
allPaths2 q db
  | (Just rs) <- db M.!? q =
      foldl' (\a (b, c) -> a + c * (1 + allPaths2 b db)) 0 rs
  | otherwise = 0

main :: IO ()
main = do
  inp <- readFile "input/day-7.txt"
  let
    bs = M.fromListWith (++) . concatMap readBag . lines $ inp
    res1 = allPaths "shiny gold" bs
    bs2 = M.fromList . map readBag2 . lines $ inp
    res2 = allPaths2 "shiny gold" bs2
  print res1
  print res2
