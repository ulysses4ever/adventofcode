#!/usr/bin/env cabal
{- cabal:
build-depends: base, megaparsec, parser-combinators, containers, extra
-}
{-# language ViewPatterns #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe
import Data.Void
import Data.List
import Control.Arrow
import Numeric
import Data.List.Extra (splitOn)
import Text.Pretty.Simple
import qualified Data.IntMap.Lazy as M
import Data.IntMap.Strict ((!))

type Str = String
type Parser = Parsec Void Str

data Rule =
  Chr Char |
  Rec [[Int]]
  deriving Show

-- Parse grammar rules

identify :: Str -> (Int, Rule)
identify s = (i, r)
  where
  [(i, ':' : ' ' : rest)] = readDec s
  r = case rest of
    '"' : c : '"' : [] -> Chr c
    _                  -> readRecRule rest

readRecRule :: Str -> Rule
readRecRule = fromJust . parseMaybe recRuleParser
  where
  alt :: Parser [Int]
  alt = L.decimal `sepEndBy` space
  
  recRuleParser :: Parser Rule
  recRuleParser = Rec <$> alt `sepBy` (chunk "| ")

-- Compile grammar rules to parsers

type ParserV = Parser ()
type RTable = [(Int,Rule)]
type PTable = M.IntMap ParserV

-- Compile a grammar rule to a parser consulting a table of already compiled rules
compileRule :: PTable -> Rule -> ParserV
compileRule _ (Chr c) =
  char c >> pure ()
compileRule t (Rec iis) = foldl1' ((<|>) . try) $ map compileAlt iis
  where
  compileAlt :: [Int] -> ParserV
  compileAlt = foldl1' (>>) . map (t !)

-- Compile a table or rules into a table parsers lazily. Mutually recursive with `compileRule`
compileTable :: RTable -> PTable
compileTable rtable = res
  where
  res = M.fromList $ map (second $ compileRule res) rtable

-- Reading the rule table

readRuleTableAndStrs :: Str -> (RTable, [Str])
readRuleTableAndStrs inp = (rtable, strs)
  where
  [lines -> grammar, lines -> strs] = splitOn "\n\n" inp
  rtable = map identify grammar

test = do
  inp <- readFile "input/day-19-test-3-part-2.txt"
  let (rtable, _strs) = readRuleTableAndStrs inp
      ptable = compileTable rtable
      p0 = ptable ! 0
      r = either errorBundlePretty show $ parse p0 "test" "aaaaabbaabaaaaababaa"
  pPrint r

-- Main

main :: IO ()
main = do
  inp <- readFile "input/day-19-test-3-part-2.txt"
  let (rtable, strs) = readRuleTableAndStrs inp
      ptable = compileTable rtable
      p0 = ptable ! 0
      ps = map (parseMaybe p0) $ strs
      res1 = length . filter isJust $ ps
      res2 = zip ps strs
  pPrint res1
  pPrint res2

