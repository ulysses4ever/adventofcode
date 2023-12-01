module Main where

import Data.Char
import Data.List (foldl', isPrefixOf)

import AoC

solve :: [String] -> Int -> Int
solve inp = \case
    1 -> part1 inp
    2 -> inp |> part2 .> part1
  where
    part1 = map getDigit .> sum
    getDigit :: String -> Int
    getDigit
      =  filter isDigit
      .> (\ds -> [head ds, last ds])
      .> read

    part2 :: [String] -> [String]
    part2 = map digitize

digitize :: String -> String
digitize = \case
  [] -> []
  ln -> let (c:ln') = tryDigitizePrefix ln in
     c : digitize ln'

tryDigitizePrefix :: String -> String
tryDigitizePrefix ln = foldl' tryPrefix ln [
          ("one","1")
          , ("two","2")
          , ("three","3")
          , ("four","4")
          , ("five","5")
          , ("six","6")
          , ("seven","7")
          , ("eight","8")
          , ("nine","9")
          ]
tryPrefix :: String -> (String, String) -> String
tryPrefix ln (w, d)
  | w `isPrefixOf` ln = head d : drop (length w) ln
  | otherwise = ln

main :: IO ()
main = defaultMain solve
