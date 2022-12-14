#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, aeson, bytestring, vector
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.Maybe
import Data.List
import Data.List.Extra (split)
import Data.Aeson
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Maybe (catMaybes)

type P = (Value, Value)

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
part :: Int -> [P] -> Int
part 1 =
  map (uncurry procPair)
  .> catMaybes
  .> zip [1..]
  .> filter snd
  .> map fst
  .> sum
part 2 =
  concatMap (\(v1,v2) -> [v1,v2])
  .> (\ps -> k1:k2:ps)
  .> sortBy (\v1 v2 -> procPair v1 v2 |> mb2ord)
  .> (\ps -> elemIndex k1 ps >>=
       \i1 -> elemIndex k2 ps >>=
       \i2 -> pure $ (i1+1) * (i2+1))
  .> fromJust
  where
    k1s = "[[2]]"
    k2s = "[[6]]"
    (k1,k2) = parseGroup [k1s, k2s]

mb2ord = \case
  Nothing -> EQ
  Just True -> LT
  Just False -> GT

procPair :: Value -> Value -> Maybe Bool
procPair (Number n1) (Number n2) = case compare n1 n2 of
  LT -> Just True
  EQ -> Nothing
  GT -> Just False
procPair v1@(Array _)  v2@(Number _) = procPair v1 (Array $ pure v2)
procPair v1@(Number _) v2@(Array _) = procPair (Array $ pure v1) v2
procPair v1@(Array a1) v2@(Array a2)
  | V.null a1 && V.null a2 = Nothing
  | V.null a1 = Just True
  | V.null a2 = Just False
  | otherwise =
    procPair (V.head a1) (V.head a2) <|>
    procPair (Array $ V.tail a1) (Array $ V.tail a2)

-- Read one line of problem's input into something more structured
parseGroup :: [String] -> P
parseGroup =
  map (decode . BSC.pack)
  .> catMaybes
  .> \[v1,v2] -> (v1, v2)

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
solve input = (part <$> [1,2]) <*> pure (parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> split (== "") .> map parseGroup
