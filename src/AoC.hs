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
  Groups(..), Lines(..),
  trace, traceShow, traceId, traceShowId,
  pPrint, pTraceShow, pTraceShowCompact, pTraceShowIdCompact,
  coerce, foldl'
  )
  where

import Debug.Pretty.Simple
import Text.Pretty.Simple (pPrint, CheckColorTty (CheckColorTty), defaultOutputOptionsDarkBg, outputOptionsCompact)
import Debug.Trace
import Data.Coerce (coerce)

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
