{-# language BangPatterns #-}
module Y2021.Day18 (solve) where

import Data.List
import Data.Either
import Debug.Trace
import GHC.HsToCore.PmCheck.Types (TmState(ts_facts))
import TcRnMonad (whenNoErrs)
import DynFlags (DynFlags(maxWorkerArgs))

-- first arg is Part #
solve :: Int -> String -> IO ()
solve n = print . compute n . parse

type E a b = Either a b
type Rec = [T]
type NU = [T]
data St = S !Int ![T] ![T]
data T = LB | RB | N Int  -- tokens
  deriving (Show, Eq)
data NP = B NP NP | L Int -- parsed numbers
  deriving Show

compute :: Int -> [Rec] -> Int
compute n rs = res n
  where
    sum = foldl1' add rs
    res 1 = magnitude sum
    res 2 = maximum [ magnitude m |
              ns <- tails rs, not $ null ns,
              let (n:ns') = ns,
              n' <- ns', m <- [add n n', add n' n]]

magnitude :: NU -> Int
magnitude = go . parseNum
  where
    go (B n1 n2) = 3 * go n1 + 2 * go n2
    go (L n) = n

parseNum :: NU -> NP
parseNum nu = head $ go nu []
  where
    go [] res = res
    go (RB : rest) (np1 : np2 : res) = go rest $ B np2 np1 : res
    go (N n : rest) res = go rest $ L n : res
    go (_lb : rest) res = go rest res

add :: NU -> NU -> NU
add n m = reduce (LB : n ++ m ++ [RB])

reduce :: NU -> NU
reduce n = case explode n of
  Left  n -> reduce n
  Right n -> case split n of
    Left n'  -> reduce n'
    Right n' -> n'

explode :: NU -> E NU NU
explode num = go (S 0 [] num)
  where
    go (S level past []) = Right $ reverse past
    go (S level past (t:ts)) = case t of
      LB -> proceed (level+1)
      RB -> proceed (level-1)
      N n -> if level > 4
        then Left $ doExplode n
        else proceed level
      where
        proceed l = go (S l (t:past) ts)
        (N m) : _rb : next = ts
        doExplode n = (reverse $ updNext (tail past) n) ++ (N 0 : updNext next m)
        updNext :: [T] -> Int -> [T]
        updNext ts n = go ts []
          where
            go [] res = res
            go (t:ts) res = case t of
              LB -> go ts (t:res)
              RB -> go ts (t:res)
              N m -> (reverse res) ++ (N $ n+m) : ts

split :: NU -> E NU NU
split = go []
  where
    go ls [] = Right $ reverse ls
    go ls (r:rs) = case r of
      (N n) | n >= 10 -> Left $ reverse ls ++ LB : N lw : N hg : RB : rs
        where
          lw = n `div` 2
          hg = n - lw
      _ -> go (r:ls) rs

showNU :: NU -> String
showNU n = unwords $ map p n
  where
    p LB = pure '['
    p RB = pure ']'
    p (N n) = show n

parse :: String -> [Rec]
parse = map readRec . lines

readRec :: String -> Rec
readRec = go []
  where
    go res [] = reverse res
    go res (c:cs) = flip go cs $ case c of
      ',' -> res
      '[' -> LB : res
      ']' -> RB : res
      n   -> (N . read $ pure n) : res

-- tests
testExplode :: Bool
testExplode = and
  [ e "[[[[[9,8],1],2],3],4]" == p "[[[[0,9],2],3],4]"
  , e "[7,[6,[5,[4,[3,2]]]]]" == p "[7,[6,[5,[7,0]]]]"
  , e "[[6,[5,[4,[3,2]]]],1]" == p "[[6,[5,[7,0]]],3]"
  , e "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" == p "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
  , e "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" == p "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
  ]
  where
    e = fromLeft undefined . explode . p
    p = readRec
