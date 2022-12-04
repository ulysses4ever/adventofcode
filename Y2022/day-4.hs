#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, megaparsec
-}
{-# language LambdaCase #-}

import Flow ((.>))
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Maybe

main  = getContents >>= solve .> print

--solve :: String -> [Int]
solve input = (part <$> [1,2]) <*> (pure $ parse input)

--parse :: String -> [String]
parse =  lines .> map (fromJust . parseMaybe asgnP)

type Parser = Parsec () String
data Seg = S Int Int
  deriving Show
segP :: Parser Seg
segP = S <$> (decimal <* char '-') <*> decimal

data Asgn = A Seg Seg
  deriving Show
asgnP :: Parser Asgn
asgnP = A <$> (segP <* char ',') <*> segP

part n = filter contained .> length

contained (A (S l1 r1) (S l2 r2)) =
  l1 <= l2 && r2 <= r1 ||
  l2 <= l1 && r1 <= r2
