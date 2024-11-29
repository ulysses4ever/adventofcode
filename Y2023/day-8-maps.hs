module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict ((!))
import Data.List (find)
import Data.Char
import AoC

solve :: (String, Lines (Words String)) -> Int -> Int
solve (cs, (Ls ls')) = \case
    1 -> 0 -- part1
    2 -> pTraceShowCompact db part2
  where
    ls :: [[String]]
    ls = coerce ls'

    db :: IntMap (Int,Int,String)
    db = IntMap.fromList $
      flip map ls \[fst, _eq, snd, trd] ->
        (c fst, (c $ tail $ init snd, c $ init trd, fst))

    part1' =
      interp db (cycle cs) 0 (c "AAA")

    part1 =
      iterateWhile
        (\(_db, _cs, node, _index) -> node == zzz)
        (\(db, (c:cs), node, !index) -> let
               (l,r, _lbl) = db ! node
               node' = case c of
                        'L' -> l
                        'R' -> r
            in (db, cs, node', index+1)
           )
        (db, cycle cs, c "AAA", 0)
      |> (\(_,_,_,index) -> index)

    endsWith c n =
      db ! n |> (\(_,_,lbl) -> last lbl == c)

    part2 = pTraceShowCompact init
      (iterateWhile
        (\(_cs, nodes, _index) -> all (endsWith 'Z') nodes)
        (\((c:cs), nodes, !index) -> let
               step node = let
                    (l,r, _lbl) = db ! node
                 in case c of
                        'L' -> l
                        'R' -> r
            in (cs, map step nodes, index+1)
           )
        (cycle cs, init, 0)
      |> (\(_,_,index) -> index))
      where
        init = IntMap.filter (\(_,_,lbl) -> last lbl == 'A') db
          |> IntMap.keys

zzz = c "ZZZ"

interp db (c:cs) index state
  | state == zzz = index
  | c == 'L' = next l
  | c == 'R' = next r
  where
    (l, r, lbl) = db ! state
    next = interp db cs (index+1)

iterateWhile ::
  (state -> Bool) ->
  (state -> state) ->
  state ->
  state
iterateWhile isFinal next current
  | isFinal current = current
  | otherwise = iterateWhile isFinal next (next current)
 

c :: String -> Int
c s = zipWith (*) [1,26,26*26] (map i s) |> sum
  where
    i c = ord c - ord 'A'

main :: IO ()
main = defaultMain solve
