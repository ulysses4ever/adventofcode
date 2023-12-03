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
  HomoGroups,
  trace, traceShow, traceId, traceShowId,
  pPrint, pTraceShow
  )
  where

import Debug.Pretty.Simple
import Text.Pretty.Simple (pPrint)
import Debug.Trace

import Flow ((.>), (|>))

import Types
import Input

defaultMain :: (Input a, Show b) => (a -> Int -> b) -> IO ()
defaultMain solve = interact (input .> (\i -> solve i <$> [1,2]) .> show)
