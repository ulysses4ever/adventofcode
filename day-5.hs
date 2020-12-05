#!/usr/bin/env cabal
{- cabal:
build-depends: base, megaparsec
-}
{-# language RecordWildCards, LambdaCase, TypeApplications #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Data.Maybe
import Data.List
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

type Str = String
type Parser = Parsec Str Str

data Rec = R {
  r :: Int,
  s :: Int }
  deriving Show

fromStr :: Str -> Rec
fromStr sr = case splitAt 7 sr of
  (r,s) -> R (read' r) (read' s)
  where 
  read' = readBin . map toBin

toBin :: Char -> Char
toBin = \case
  'F' -> '0'
  'B' -> '1'
  'L' -> '0'
  'R' -> '1'
  _   -> error "toBin"

readBin :: Str -> Int
readBin = fromJust . parseMaybe @() L.binary

toId :: Rec -> Int
toId R{..} = r * 8 + s

main :: IO ()
main = do
  inp <- readFile "input/day-5.txt"
  let recs = map fromStr $ lines  inp
      ids = map toId recs
      sids = sort ids
      -- res1 = maximum ids
      res2 = 1 + (snd . head . dropWhile (\(c, p) -> c == p+1) $ (zip (tail sids) sids))
  print res2

