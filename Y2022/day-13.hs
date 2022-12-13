#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, aeson, bytestring, vector
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List
import Data.List.Extra (split)
import Data.Aeson
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Maybe (catMaybes)

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n = --zip ps
  map procPair
  .> catMaybes
  .> zip [1..]
  .> filter snd
  .> map fst
  .> sum
  -- where
  --   rs = map procPair ps

type P = (Value, Value)

procPair :: [Value] -> Maybe Bool
procPair [Number n1, Number n2] = case compare n1 n2 of
  LT -> Just True
  EQ -> Nothing
  GT -> Just False
procPair [v1@(Array _), v2@(Number _)] = procPair [v1, Array $ pure v2]
procPair [v1@(Number _), v2@(Array _)] = procPair [Array $ pure v1, v2]
procPair [v1@(Array a1), v2@(Array a2)]
  | V.null a1 && V.null a2 = Nothing
  | V.null a1 = Just True
  | V.null a2 = Just False
  | otherwise =
    procPair [V.head a1, V.head a2] <|>
    procPair [Array $ V.tail a1, Array $ V.tail a2]
procPair _ = Just True

-- Read one line of problem's input into something more structured
parseGroup :: [String] -> [Value]
parseGroup = map (decode . BSC.pack) .> catMaybes

{--------------------------------------------------------
--
-- Below is somewhat standard stuff, which is moslty reused
-- from day to day.
--
--------------------------------------------------------}

-- Entry point: read stdin, solve the problem and print the result
main  = interact (solve .> map show .> unlines)

-- Solve both parts and return a list with two elements -- the results
-- Input: problem's full text
solve input = (part <$> [1]) <*> pure (parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> split (== "") .> map parseGroup
