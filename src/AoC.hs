{-# language TypeSynonymInstances #-}
{-# language DerivingStrategies   #-}
{-# language GeneralizedNewtypeDeriving   #-}

module AoC where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Flow ((.>)) --, (|>))
import Data.List.Extra (split)
import Data.Maybe (fromJust)

class Input a where
  input :: String -> a

type Str = BS.ByteString

instance Input Str where
  input = BSC.pack

newtype LineGroups a = LineGroups [[a]]
  deriving newtype Show

instance Input a => Input (LineGroups a) where
  input = lines .> split (== "") .> map (map input) .> LineGroups

instance Input Value where
  input = BSC.pack .> BSC.fromStrict .> decode .> fromJust
