#!/usr/bin/env cabal
{- cabal:
build-depends: base, megaparsec, parser-combinators, extra
-}
{-# language RecordWildCards, LambdaCase, TypeApplications, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Data.Bool
import Data.Char
import Data.Functor (void)
import Data.Maybe
import Data.Either
import Data.List
import Data.List.Extra (nubOrd)
import Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char

type Str = String
type Field = (Str,Str)
type Parser = Parsec Str Str

data Rec = R {
  fs :: [Field] }
  deriving Show

fieldSep :: Str
fieldSep = " \n"

recSep :: Str
recSep = "\n\n"

inpToRecs :: Str -> [Rec]
inpToRecs =
  --fromJust . parseMaybe  -- does not work (returns Nothing) for some reason??
  fromRight [] . runParser inpParser "day-4"

inpParser :: Parser [Rec]
inpParser = recParser `sepBy` (string recSep)

recParser :: Parser Rec
recParser =
  R <$>
    fieldParser `sepBy`
    (try $ oneOf fieldSep >> (lookAhead $ noneOf "\n"))
    -- ^ this is ugly lookahead b/c `recSep` looks like a `fieldSep` at start

fieldParser :: Parser Field
fieldParser = do
  fname <- some . noneOf $ ":"
  void $ single ':'
  fval  <- some $ noneOf fieldSep
  return (fname, fval)

-- the whole Validation business (instead of simple Bool) was due to painfull
-- debugging and not caring enough to clean it up (well, it's more robust than Bool...)
type Validation = Either String ()

recValid1 :: Rec -> Bool
recValid1 R{..} =
  (n == 8 ||
    (n == 7 && not ("cid" `elem` (map fst fsu))))
  where
  fsu = nubOrd fs
  n = length fsu

-- Empty list if first-part check doesn't hold
recValid :: Rec -> [Validation]
recValid r@R{..} =
  bool
    []
    (map fieldValid (sortOn fst fs))
    (recValid1 r)

fieldValid :: Field -> Validation
fieldValid (n, v) = check $ case n of

  "byr" -> checkYear 1920 2002

  "iyr" -> checkYear 2010 2020

  "eyr" -> checkYear 2020 2030

  "hgt" ->
    let (reverse -> units, reverse -> ht) = splitAt 2 $ reverse v
    in case units of
      "cm" -> checkHeight 150 193 ht

      "in" -> checkHeight 59 76 ht

      _    -> False

  "hcl" -> isJust $
    parseMaybe @() (char '#' >> count 6 (digitChar <|> oneOf "abcdef")) v

  "ecl" -> (v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

  "pid" -> length v == 9 && all isDigit v

  "cid" -> True

  _     -> False

  where
  ok = Right ()

  invalid = Left $ n ++ ":" ++ v

  check = bool invalid ok

  checkYear :: Int -> Int -> Bool
  checkYear from to =
    length v == 4 &&
      maybe False (\y -> from <= y && y <= to) (readMaybe @Int v)

  checkHeight :: Int -> Int -> Str -> Bool
  checkHeight from to =
    maybe False (\h -> from <= h && h <= to) . readMaybe @Int

main :: IO ()
main = do
  inp <- readFile "input/day-4.txt"
  let recs = inpToRecs inp
      recsSort = map (\r -> r {fs = sortOn fst $ fs r}) recs -- to ease debugging
      vs = map recValid recsSort
      res1 = length $ filter recValid1 recs
      res2 = length . filter (all isRight) . filter (not . null)  $ vs
  print res1
  print res2

