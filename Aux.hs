module Aux where

import Data.List.Extra (split)

readIntsSep :: Char -> String -> [Int]
readIntsSep c = map read . split (== c)
