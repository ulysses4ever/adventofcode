#!/usr/bin/env cabal
{- cabal:
build-depends: base, pretty-simple
-}
{-# language RecordWildCards, TypeApplications, EmptyCase #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Text.Read
import Text.Pretty.Simple

type Str = String

-------------------- Part 1

data DirS = North | East | South | West
  deriving (Eq, Ord, Enum, Show, Read, Bounded)

data St = St {
  e :: Int,
  n :: Int,
  d :: DirS }
  deriving Show

data Dir = N | E | S | W | L | R | F
  deriving (Eq, Ord, Enum, Show, Read, Bounded)

data Cmd = C {
  cd :: Dir,
  len :: Int }

start :: St
start = St 0 0 East

ndirs :: Int
ndirs = fromEnum (maxBound @DirS) + 1

parse :: Str -> Maybe Cmd
parse [] = Nothing
parse (c : s) = do
  cd <- readMaybe @Dir [c]
  len <- readMaybe @Int s
  pure C{..}

exec :: St -> Cmd -> St
exec s@St{..} C{..} = case cd of
  N -> s { n = n + len}
  S -> s { n = n - len}
  E -> s { e = e + len}
  W -> s { e = e - len}
  L -> s { d = adjustDirSLeft d (len `div` 90) }
  R -> s { d = adjustDirSRight d (len `div` 90) }
  F -> forward s len

adjustDirSLeft, adjustDirSRight :: DirS -> Int -> DirS

adjustDirSLeft d n = toEnum $
  (fromEnum d + ndirs - n) `mod` ndirs

adjustDirSRight d n = toEnum $
  (fromEnum d + n) `mod` ndirs

forward :: St -> Int -> St
forward s@St{..} len = case d of
  North -> s {n = n + len}
  South -> s {n = n - len}
  East  -> s {e = e + len}
  West  -> s {e = e - len}

-- Part 2
data WP = WP {
  ew :: Int,
  nw :: Int }
  deriving Show

data St2 = St2 {
  e2 :: Int,
  n2 :: Int,
  w  :: WP }
  deriving Show

start2 :: St2
start2 = St2 0 0 (WP 10 1)

exec2 :: St2 -> Cmd -> St2
exec2 s@St2{w=w@WP{..},..} C{..} = case cd of
  N -> s { w = w {nw = nw + len} }
  S -> s { w = w {nw = nw - len} }
  E -> s { w = w {ew = ew + len} }
  W -> s { w = w {ew = ew - len} }
  L -> s { w = adjustWPLeftN w (len `div` 90) }
  R -> s { w = adjustWPRightN w (len `div` 90) }
  F -> s { n2 = n2 + len * nw, e2 = e2 + len * ew }

adjustWPLeft, adjustWPRight :: WP -> WP
adjustWPRight w@WP{..}= w {nw = -ew, ew = nw}
adjustWPLeft  w@WP{..}= w {nw = ew, ew = -nw}

adjustWPLeftN, adjustWPRightN :: WP -> Int -> WP
adjustWPLeftN w len = iterate adjustWPLeft w !! len
adjustWPRightN w len = iterate adjustWPRight w !! len

-- Main

getInput :: IO [String]
getInput =
  lines <$> readFile "input/day-12.txt"

main :: IO ()
main = do
  inp <- getInput
  let pinp = mapM parse inp
      (Just final) = foldl exec start <$> pinp
      res1 = abs (e final) + abs (n final)

      (Just final2) = foldl exec2 start2 <$> pinp
      res2 = abs (e2 final2) + abs (n2 final2)
  -- ~ pPrint final
  print res1
  pPrint res2
