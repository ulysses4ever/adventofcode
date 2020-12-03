#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}
{-# language RecordWildCards #-}
module Main where

import Data.List (unfoldr)

type Pos  = Int
type Cell = Char
type Str  = String
type Map  = [Str]
type Slope = (Int, Int)

data State = S {
  posX :: Pos,
  posY :: Pos,
  map  :: Map
  }

startState :: Map -> State
startState = S 0 0

getCell :: Map -> Pos -> Pos -> Cell
getCell m x y = m !! y !! (x `mod` n)
  where
  n = length (m !! 0)

takeStep :: Slope -> State -> Maybe (Cell, State)
takeStep (dx, dy) S{..} = 
  if posY' >= length map 
    then Nothing
    else Just (cell, S posX' posY' map)
  where 
  posX' = posX + dx
  posY' = posY + dy
  cell = getCell map posX' posY'

slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

main ::  IO ()
main = do
  cts <- readFile "day-3-input.txt"
  let score = length . filter (== '#')
      path sl = unfoldr (takeStep sl) . startState $ lines cts

      res1 = score . path $ slopes !! 1
      res2 = product $ score . path <$> slopes
  print res1
  print res2

