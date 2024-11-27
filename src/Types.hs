{-# language PatternSynonyms      #-}

module Types where

import Linear.V2
import Linear.V3

type P = V2 Int
pattern P a b = V2 a b

type P3 = V3 Int
pattern P3 a b c = V3 a b c
