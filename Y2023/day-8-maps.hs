module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict ((!))
import Data.List (find)
import Data.Char
import AoC

solve :: (String, Lines (Words String)) -> Int -> Int
solve (cs, Ls ls') = \case
    1 -> part1 (== c "ZZZ") (c "AAA")
    2 -> part2
  where
    ls :: [[String]]
    ls = coerce ls'

    db :: IntMap (Int,Int,String)
    db = IntMap.fromList $
      flip map ls \[fst, _eq, snd, trd] ->
        (c fst, (c $ tail $ init snd, c $ init trd, fst))

    part1 isFinal init =
      iterateWhile
        (\(_cs, node, _index) -> isFinal node)
        (\(c:cs, node, !index) -> let
             (l,r, _lbl) = db ! node
             node' = case c of
                      'L' -> l
                      'R' -> r
          in (cs, node', index+1)
           )
        (cycle cs, init, 0)
      |> (\(_,_,index) -> index)

    endsWith c n =
      db ! n |> (\(_,_,lbl) -> last lbl == c)

    init2 = IntMap.filter (\(_,_,lbl) -> last lbl == 'A') db
      |> IntMap.keys

    part2 = map (part1 (endsWith 'Z')) init2
      |> foldl' lcm 1

zzz = c "ZZZ"

-- encode the string as an Int
c :: String -> Int
c s = zipWith (*) [1,26,26*26] (map i s) |> sum
  where
    i c = ord c - ord 'A'

main :: IO ()
main = defaultMain solve
