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

recValid :: Rec -> [Validation]
recValid r@R{..} = 
  if recValid1 r
    then map fieldValid (sortOn fst fs)
    else []

fieldValid :: Field -> Validation
fieldValid f = case fst f of

  "byr" -> check f $ checkYear 1920 2002

  "iyr" -> check f $ checkYear 2010 2020

  "eyr" -> check f $ checkYear 2020 2030

  "hgt" -> check f $ \v ->
    let (reverse -> units, reverse -> val) = splitAt 2 $ reverse v
    in case units of

      "cm" ->
        maybe False (\h -> 150 <= h && h <= 193) (readMaybe @Int val)
      
      "in" ->
        maybe False (\h -> 59 <= h && h <= 76)   (readMaybe @Int val)

      _    -> False

  "hcl" -> check f $ \v ->
    isJust $ parseMaybe @() (char '#' >> count 6 (digitChar <|> oneOf "abcdef")) v

  "ecl" -> check f $ \v ->
    v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    
  "pid" -> check f $ \v ->
    length v == 9 && all isDigit v
  
  "cid" -> ok
  
  _     -> invalid ("[unknown]" ++ fst f) (snd f)

  where
  ok = Right ()

  invalid n v = Left $ n ++ ":" ++ v

  check (n,v) p = bool (invalid n v) ok (p v)

  checkYear :: Int -> Int -> Str -> Bool
  checkYear from to ystr =
    length ystr == 4 && 
      maybe False (\y -> from <= y && y <= to) (readMaybe @Int ystr)

main :: IO ()
main = do
  inp <- readFile "input/day-4.txt"
  let recs = inpToRecs inp
      recsSort = map (\r -> r {fs = sortOn fst $ fs r}) recs -- to ease debugging
      vs = map recValid recsSort
      res = length . filter (all isRight) . filter (not . null)  $ vs
  print res

