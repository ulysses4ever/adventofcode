module Main where

import Control.Monad
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AoC
import Data.List (find)
import Data.Maybe (isNothing)

type S = String
type Parser = Parsec Void S
type Game = (Int, [Draws])
type Draws = [Draw]
type Draw = (Color, Int)
type Color = S

solve :: String -> Int -> Int
solve inp = \case
    1 -> part1
    2 -> part2
  where
    gs :: [Game]
    Just gs = parseMaybe (many parseGame) inp
    part1 = let
      vgs :: [Game]
      vgs = filter viable gs
      in map fst vgs |> sum
    part2 = map score gs |> sum

-- Part 2

score :: Game -> Int
score (_, dds) = product mxs
  where
    ds :: Draws
    ds = concat dds
    mxs :: [Int]
    mxs = map (fst .> (flt .> map snd .> maximum)) bounds
    flt :: Color -> Draws
    flt c = filter (fst .> (== c)) ds

-- Part 1

viable :: Game -> Bool
viable (n, dss) = concat dss |> find violator .> isNothing

violator :: Draw -> Bool
violator (c, n) = let (Just mx) = lookup c bounds in n > mx

bounds :: [(Color, Int)]
bounds =
  [ ("red",   12)
  , ("green", 13)
  , ("blue",  14)
  ]

-- Parsers

parseGame :: Parser Game
parseGame = do
  string "Game "
  n <- L.decimal
  string ": "
  ds <- tok $ sepBy parseDraws (string "; ")
  pure (n, ds)

parseDraws = sepBy parseDraw (string ", ")

parseDraw :: Parser Draw
parseDraw = do
  n <- tok L.decimal
  color <- tok $ some letterChar
  pure (color, n)

tok = L.lexeme space

main :: IO ()
main = defaultMain solve
