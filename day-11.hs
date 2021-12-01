#!/usr/bin/env cabal
{- cabal:
build-depends: base, vector, primitive, extra, ansi-terminal
-}
{-# language TupleSections, BlockArguments, MultiWayIf, ImplicitParams,
             ConstraintKinds #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import System.Console.ANSI
import System.Time.Extra
import Data.Bool
import Data.Maybe
import Data.Functor ((<&>))
import Control.Monad hiding (void)
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VV
import qualified Data.Vector.Unboxed.Mutable as VM
--import qualified Data.Vector.Generic as GV

-- container
-- rows, cols, table as a map Int -> Char
type C = V.MVector (PrimState IO) Char
type CPure = V.Vector Char
type CL = (Int, Int, CPure)
type CtxNoNb = (?rw :: Int, ?cl :: Int)
type Ctx = (?rw :: Int, ?cl :: Int, ?ns :: Ns)

-----------------   generic aux stuff

(!) :: C -> Int -> IO Char
(!) = VM.read

(<~) :: C -> Int -> Char -> IO ()
(<~) = VM.write

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-----------------  task-specific aux stuff

-- print the field
prt ::
  Ctx =>
  C -> IO ()
prt v = foldM_ s () is
  where
  s _ i = do
    st <- v ! i
    putChar st
    when ((i+1) `mod` ?cl == 0) $ putChar '\n'

-- 1-dimensional indices for a field of size rw x cl
is ::
  CtxNoNb =>
  [Int]
is = [0 .. ?rw * ?cl - 1]

posToAbs ::
  CtxNoNb =>
  (Int, Int) -> Int
posToAbs (ri, ci) = ri * ?cl + ci

absToPos ::
  CtxNoNb =>
  Int -> (Int, Int)
absToPos i = i `divMod` ?cl

free, took, void :: Char
free = 'L'
took = '#'
void = '.'

-------------------- Part 1

animate :: Bool
animate = False

loop ::
  Ctx =>
  C -> C -> IO ()
loop ci co = do
  when animate do
    clearScreen
    prt ci
    sleep 1
  changed <- iter ci co
  if changed
    then loop co ci
    else pure ()

iter ::
  Ctx =>
  C -> C -> IO Bool
iter ci co = foldM (upd ci co) False is

upd ::
  Ctx =>
  C -> C -> Bool -> Int -> IO Bool
upd ci co changed i = do
  c <- ci ! i
  if c == void then pure changed else do
    w <- weight ci i
    let (c', changed') = if
          | c == free ->
              if w == 0 then (took, True) else (free, False)
          | c == took ->
              if w >= 4 then (free, True) else (took, False)
          | True -> error "wow"
    co <~ i $ c'
    pure (changed || changed')

-- Number of places taken around
weight ::
  Ctx =>
  C -> Int -> IO Int
weight v i = do
      l  <- bool (v ! (i - 1))       mfr isLft
      r  <- bool (v ! (i + 1))       mfr isRgt
      u  <- bool (v ! (i - ?cl))     mfr isTop
      d  <- bool (v ! (i + ?cl))     mfr isBot
      ul <- bool (v ! (i - 1 - ?cl)) mfr (isLft || isTop)
      ur <- bool (v ! (i + 1 - ?cl)) mfr (isRgt || isTop)
      dl <- bool (v ! (i - 1 + ?cl)) mfr (isLft || isBot)
      dr <- bool (v ! (i + 1 + ?cl)) mfr (isRgt || isBot)

      let ind = [l,r, u,d, ul, ur, dl, dr]
      pure $ count took ind
  where
  mfr = pure free
  isTop = ri == 0
  isBot = ri == ?rw - 1
  isLft = ci == 0
  isRgt = ci == ?cl - 1
  (ri, ci) = absToPos i

-- Part 2: extended n-hood

type N = V.Vector Int
type P = (Int,Int)
type Ns = VV.Vector N

weight2 ::
  Ctx =>
  C -> Int -> IO Int
weight2 v i = undefined
  where
  nh = ?ns VV.! i
  took_cnt = V.length $ V.filter (== took) $ V.map (v !) nh

nhood ::
  (?rw :: Int, ?cl :: Int) =>
  CPure -> Int -> N
nhood v i = res
  where
  posi = absToPos i
  ps = paths posi
  res = V.fromList . catMaybes $ map firstValidM ps

  firstValidM :: [P] -> Maybe Int
  firstValidM path = let
    (p:_) = dropWhile (\p' -> isvalid p' && isvoid p') path
    in
      if isvalid p
        then Just (posToAbs p)
        else Nothing

  isvalid (ri, ci) =
    0 <= ri && ri < ?rw &&
      0 <= ci && ci < ?cl

  isvoid p = v V.! (posToAbs p) == void

paths :: P -> [[P]]
paths (ri, ci) =
  [
    [(ri + n*dr, ci + n*dc) | n<-[1..]] |
    dr<-dirs,
    dc<-dirs,
    dr /= 0 || dc /= 0
  ]
  where
  dirs = [-1..1]


-- Main

getInput :: IO CL
getInput = do
  cts <-  readFile "input/day-11.txt"
  let ls = lines cts
      cols = length $ ls !! 0
      rows = length ls
      cts' = filter (/= '\n') cts
  return (rows, cols, V.fromList cts')

main :: IO ()
main = do
  (rw, cl, inp) <- getInput
  inp1 <- V.thaw inp
  inp2 <- V.thaw inp -- V.unsafeThaw?
  res1 <-
    let ?rw = rw
        ?cl = cl
    in let
        ?ns = VV.fromList $ map (nhood inp) is
    in do
        loop inp1 inp2
        res1' <- foldM
          (\c i -> inp1 ! i <&> \s -> c + bool 0 1 (s==took))
          (0::Int)
          is
        pure res1'

      --res2 =
  print res1
