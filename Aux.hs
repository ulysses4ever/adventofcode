module Aux where

import Data.List.Extra (split)

readIntsSep :: Char -> String -> [Int]
readIntsSep c = map read . split (== c)

sign :: Int -> Int
sign n
  | n > 0 = 1
  | n < 0 = -1
  | True  = 0
