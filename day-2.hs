#!/usr/bin/env cabal
{- cabal:
build-depends: base, megaparsec
-}
{-# language RecordWildCards #-}
module Main where

import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Str = String

data Rec = R {
  f :: Int,
  t :: Int,
  c :: Char,
  s :: Str }

lineToRec :: Str -> Rec
lineToRec = fromJust . parseMaybe recParser

-- a parser for strings like: "5-14 z: vcmggppvzmzwkn"
recParser :: Parsec () Str Rec
recParser = do
  f <- L.decimal
  single '-'
  t <- L.decimal
  single ' '
  c <- anySingle
  chunk ": "
  s <- takeRest
  pure $ R f t c s
  
recValid :: Rec -> Bool
recValid R{..} = f <= n && n <= t 
  where
  n = length $ filter (== c) s

recValid2 :: Rec -> Bool
recValid2 R{..} = (fc == c) /= (tc == c)
  where
  fc = s !! (f - 1)
  tc = s !! (t - 1)
  
main ::  IO ()
main = do
  cts <- readFile "day-2-input.txt"
  let res = length $ filter (recValid2 . lineToRec) (lines cts)
  print res

