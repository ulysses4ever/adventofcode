-- | 

module Maps where

import Data.Map.Strict ( Map, (!) )
import qualified Data.Map.Strict as Map
import Flow ((.>), (|>))

import Types

newtype Map2D = Map2D (Map P Char)

readMap2DRaw :: String -> [(P, Char)]
readMap2DRaw s = do
    (r, rw) <- zip [0..] (lines s)
    (c, x)  <- zip [0..] rw
    pure (P r c, x)

readMap2D = Map2D . Map.fromList . readMap2DRaw

readMap2DNoDots :: String -> Map2D
readMap2DNoDots = readMap2DRaw
  .> filter (\(_,c) -> c == '.')
  .> Map.fromList
  .> Map2D

nhood4 p = map (+ p) [P -1 0, P 0 -1, P 1 0, P 0 1]
