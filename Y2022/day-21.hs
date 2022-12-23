#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra, containers
-}
{-# language LambdaCase #-}
{-# language TupleSections #-}

import Flow ((.>), (|>))
import Data.List
import Data.Maybe
import qualified Data.Map as M

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n inp = res n
  where
    res 1 = prog M.! "root"
    res 2 = go root2humn (prog M.! first)
      where
        [opd1, op, opd2] = vars M.! "root"
        first | head root2humn == opd1 = opd2
              | otherwise = opd1
        go ["humn"] val = val
        go (v:v':vs) val = go (v':vs) val'
          where
            [opd1, op, opd2] = vars M.! v
            (knw, dir)
              | opd1 == v' = (opd2, Left ())
              | opd2 == v' = (opd1, Right ())
              | otherwise = error ("Unknown operand: " ++ v)
            val' = inv op dir val (prog M.! knw)

    -- part 1
    prog = M.fromList $ map interp inp
    interp = \case
      (v, [n]) -> (v, read n :: Int)
      (v, [opd1, op, opd2]) -> (v, (parseOp op) (prog M.! opd1) (prog M.! opd2))

    -- part 2
    vars = M.fromList inp
    refs = M.fromList . concat . catMaybes $ unfoldr (\case
        [] -> Nothing
        (v, assoc) : rest -> Just . (,rest) $ case assoc of
          [n] -> Nothing
          [opd1, op, opd2] -> Just [(opd1, v), (opd2, v)]
        ) inp
    root2humn = go "humn" []
      where
        go r@"root" path = path
        go v path = go (refs M.! v) (v:path)

inv op dir val knownOpd
  | op == "+" || op == "*" || dir == Left () = invOp op val knownOpd
  | dir == Right () = parseOp op knownOpd val
  where
    invOp = \case
      "+" -> (-)
      "-" -> (+)
      "*" -> div
      "/" -> (*)

parseOp = \case
  "+" -> (+)
  "-" -> (-)
  "*" -> (*)
  "/" -> div

-- Read one line of problem's input into something more structured
-- parseLine :: String -> ???
parseLine = words .> \case v:rest -> (init v, rest)

{--------------------------------------------------------
--
-- Below is somewhat standard stuff, which is moslty reused
-- from day to day.
--
--------------------------------------------------------}

-- Entry point: read stdin, solve the problem and print the result
main  = interact (solve .> show)

-- Solve both parts and return a list with two elements -- the results
-- Input: problem's full text
solve input = (part <$> [1,2]) <*> pure (parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> map parseLine
