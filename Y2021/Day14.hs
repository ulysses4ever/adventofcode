{-# language BangPatterns #-}
module Y2021.Day14 (solve) where

import Aux
import Data.List
import Debug.Trace
import qualified Data.Map.Strict as M

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type I = Int
type Pat = (Char, Char)
type Rules = M.Map Pat Char
type St = M.Map Pat I

compute :: Int -> (String, Rules) -> I
compute n (t, rs) = res
  where
    iters = if n == 1 then 10 else 40
    init = initState t
    final = iterate (step rs) init !! iters
    scores = map snd $ decode (insInc ('?', head t) final) -- correct for the first char in t
    mx = maximum scores
    mn = minimum scores
    res = mx - mn

step :: Rules -> St -> St
step rs = M.foldlWithKey' upd M.empty
  where
    upd s' p@(l,r) n = case p `M.lookup` rs of
      Just c  -> insAdd (c,r) n
                   (insAdd (l,c) n s')
      Nothing -> s'

decode :: St -> [(Char,I)]
decode = M.toList . M.foldlWithKey' (\res (_,c) n -> insAdd c n res) M.empty

initState :: String -> St
initState t = foldl' (flip insInc) M.empty (zip t $ tail t)

insAdd :: Ord k => k -> I -> M.Map k I -> M.Map k I
insAdd = M.insertWith (+)
insInc :: Ord k => k -> M.Map k I -> M.Map k I
insInc k = insAdd k 1

parse :: String -> (String, Rules)
parse i = (template, M.fromList $ map readRule rules)
  where
    template : empty : rules = lines i

readRule :: String -> (Pat, Char)
readRule r@(a:b:_) = ((a,b), last r)
