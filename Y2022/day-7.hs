#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List
import Data.List.Extra (nubOrdOn)
import Data.Maybe

type Name = String
type File = (Name, Int)
type Dir  = [Either Name File] -- either name of a subdir a file
type Tree = [(Name, Dir)] -- map name of a dir to its description
type State = (Tree, Dir, Name)

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n i = map (size tree) dirs |> filter (<= 100000) |> sum
  where
    tree' = buildTree i
    tree  = nubOrdOn fst tree'
    dirs = map fst tree

size :: Tree -> Name -> Int
size tree name = lookup name tree |> fromJust |> map getSize |> sum
  where
    getSize = \case
      (Right (fname, fsize)) -> fsize
      (Left dname) -> size tree dname

buildTree = tail .> foldl' collect ([], [], "/") .> complete
  where
    complete :: State -> Tree
    complete (tree, dir, name) = if null dir then tree else (name, dir) : tree

    collect :: State -> [String] -> State
    collect s@(tree, dir, nameCurrent) = \case
      ["$", "ls"] -> s

      ["$", "cd", ".."] -> s

      ["$", "cd", name] ->
        (complete s, [], name)

      ["dir", name] -> (tree, (Left name) : dir, nameCurrent)

      [size, name] -> (tree, (Right (name, read size)) : dir, nameCurrent)

-- Read one line of problem's input into something more structured
-- parseLine :: String -> ???
parseLine = words

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
solve input = (part <$> [1]) <*> (pure $ parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> map parseLine
