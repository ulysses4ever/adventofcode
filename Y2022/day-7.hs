#!/usr/bin/env cabal
{- cabal:
build-depends: base, flow, extra
-}
{-# language LambdaCase #-}

import Flow ((.>), (|>))
import Data.List
import Data.Maybe
import Debug.Trace

type Name = String
type File = (Name, Int)
type Dir  = [Either Name File] -- either name of a subdir a file
type Path = [Name]
type Tree = [(Path, Dir)] -- map name of a dir to its description
type State = (Tree, Dir, Path)

-- Solve part n (n is 1 or 2) of the problem: turn structured input into the result
-- part :: Int -> ??? -> Int
part n i = res n
  where
    res 1 = sizes |> filter (<= 100000) |> sum
    res 2 = head $ dropWhile (< need) $ sort sizes

    sizes = map (size tree) dirs
    tree = buildTree i
    dirs = map fst tree

    use = last sizes
    req = 30000000
    ava = 70000000
    fre = ava - use
    need = req - fre

size :: Tree -> Path -> Int
size tree path = lookup path tree
  |> fromMaybe (trace ("Unknown path: " ++ (intercalate "/" (reverse path))) [])
  |> map getSize
  |> sum
  where
    getSize = \case
      (Right (fname, fsize)) -> fsize
      (Left dname) -> size tree (dname : path)

buildTree = tail .> foldl' collect ([], [], ["/"]) .> complete
  where
    complete :: State -> Tree
    complete (tree, dir, path) = if null dir then tree else (path, dir) : tree

    collect :: State -> [String] -> State
    collect s@(tree, dir, path) = \case
      ["$", "ls"] -> s

      ["$", "cd", ".."] -> (complete s, [], tail path)

      ["$", "cd", name] ->
        (complete s, [], name:path)

      ["dir", name] -> (tree, (Left name) : dir, path)

      [size, name] -> (tree, (Right (name, read size)) : dir, path)

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
solve input = (part <$> [1,2]) <*> (pure $ parse input)

-- Turn problem's full text into something more structured
-- parse :: String -> ???
parse =  lines .> map parseLine
