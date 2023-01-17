#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, search-algorithms, linear, containers, transformers
-}
{-# language LambdaCase #-}
{-# language BlockArguments #-}
{-# language PatternSynonyms #-}

import Flow ((.>), (|>))
import Data.List
import Algorithm.Search
import Linear.Vector
import Linear.V2
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import Control.Monad.Trans.State
import Control.Monad (filterM)
import Debug.Trace

type P = V2 Int
pattern P x y = V2 x y

type El = Char
type C = [[El]]
type M = M.Map P Int
type Path = [P]

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part 1 inp = -- traceShow sp1
  res
  where
    sp1 = sp inp (initial inp)
    res = sp1 |> length
part 2 inp = min
  where
    is = initial inp : allOtherInitials inp
    (_, min) = foldl' upd (M.empty, maxBound) is
    upd s@(prev, min) i = case sp2 i prev min inp of
      Nothing -> s
      Just (path, min') -> -- traceShow path
        (updPrev prev (i:path), min')
    updPrev prev path = prev `M.union`
      (M.fromList $ zip (reverse path) [0..])

sp2 :: P -> M -> Int -> C -> Maybe (Path, Int)
sp2 start prev min inp
  | Just path <- mPath = let len = length path in
      if len < min
        then Just (path, len)
        else Nothing
  | otherwise = Nothing
  where
    (mPath, _steps) = flip runState (M.singleton start (0::Int)) $ bfsM
      next
      (\p -> pure $ getEl inp p == 'E')
      start
    next p = do
      steps <- get
      let stepsToHere = steps M.! p -- ought to exist
          ns = neighbors p
            |> filter inBounds
            |> filter (elevateOk p)
          minFromHere = M.findWithDefault 0 p prev
          busted = stepsToHere + minFromHere >= min
      put $ steps `M.union` (M.fromList . zip ns $ repeat (stepsToHere + 1))
      pure if busted then [] else ns
    (elevateOk, inBounds) = tests inp

tests inp = (elevateOk, inBounds)
  where
    elevateOk p p' = elev p' <= elev p + 1
    elev p = ord case getEl inp p of
          'S' -> 'a'
          'E' -> 'z'
          e   -> e
    (P rmx cmx) = size2 inp
    inBounds (P x y) = 0 <= x && x < rmx &&
      0 <= y && y < cmx

sp :: C -> P -> [P]
sp inp start = bfs
      next
      (\p -> getEl inp p == 'E')
      start
      |> fromJust
  where
    next p = neighbors p
      |> filter inBounds
      |> filter (elevateOk p)
    (elevateOk, inBounds) = tests inp

getEl :: C -> P -> El
getEl cont (P r c) = cont !! r !! c

allOtherInitials :: C -> [P]
allOtherInitials inp =
  map (uncurry P . (`divMod` (length $ head inp))) inds
  where
    inds = elemIndices 'a' (concat inp)

initial :: C -> P
initial inp = P (length pre) (findIndex (== 'S') tar |> fromJust)
  where
    (pre, tar:post) = span ('S' `notElem`) inp

size2 :: [[a]] -> P
size2 m = P (length m) (length $ head m)

neighbors p = map (p +) basis ++ map (p -) basis

-- Read one line of problem's input into something more structured
-- parseLine :: String -> ???
parseLine = id

{--------------------------------------------------------
--
-- Below is somewhat standard stuff, which is moslty reused
-- from day to day.
--
--------------------------------------------------------}

-- Entry point: read stdin, solve the problem and print the result
main  = interact (solve .> show)

-- Solve both parts and return a list with two elements -- the results
-- Input: problem's full text
solve input = (part <$> [1,2]) <*> pure (parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> map parseLine
