{-# language TypeSynonymInstances #-}
{-# language DerivingStrategies   #-}
{-# language LambdaCase           #-}
{-# language FlexibleInstances    #-}

module AoC (
  (.>), (|>),
  defaultMain,
  P, P3,
  Input(input),
  HomoGroups,
  trace, traceShow, pPrint
  )
  where

import Text.Pretty.Simple (pPrint)
import Debug.Trace

import Flow ((.>), (|>))

import Types
import Input

defaultMain :: (Input a, Show b) => (a -> Int -> b) -> IO ()
defaultMain solve = interact (input .> (\i -> solve i <$> [1,2]) .> show)
