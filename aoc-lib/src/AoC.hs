{-# language TypeSynonymInstances #-}
{-# language DerivingStrategies   #-}
{-# language LambdaCase           #-}
{-# language FlexibleInstances    #-}

module AoC (
  (.>), (|>),
  defaultMain,
  P, P3,
  pattern P, pattern P3,
  Input(input),
  Groups(..), Lines(..), Words(..),
  trace, traceShow, traceId, traceShowId,
  pPrint, pTraceShow, pTraceShowCompact, pTraceShowIdCompact,
  coerce, foldl',
  iterateWhile,
  counter, genericCounter
  )
  where

import Debug.Pretty.Simple
import Text.Pretty.Simple (pPrint, CheckColorTty (CheckColorTty), defaultOutputOptionsDarkBg, outputOptionsCompact)
import Debug.Trace
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap

import Flow ((.>), (|>))

import Types
import Input
import Data.List (foldl')

pTraceShowIdCompact :: (Show a) => a -> a
pTraceShowIdCompact x = pTraceShowCompact x x

pTraceShowCompact :: (Show a) => a -> b -> b
pTraceShowCompact
  = pTraceShowOpt
      CheckColorTty
      (defaultOutputOptionsDarkBg { outputOptionsCompact = True })

defaultMain :: (Input a, Show b) => (a -> Int -> b) -> IO ()
defaultMain solve = interact (input .> (\i -> solve i <$> [1,2]) .> show)

iterateWhile ::
  (state -> Bool) ->
  (state -> state) ->
  state ->
  state
iterateWhile isFinal next current
  | isFinal current = current
  | otherwise = iterateWhile isFinal next (next current)

counter :: [Int] -> IntMap.IntMap Int
counter = flip zip [1,1..] .> IntMap.fromListWith (+)

genericCounter :: Ord a => [a] -> Map.Map a Int
genericCounter = flip zip [1,1..] .> Map.fromListWith (+)
