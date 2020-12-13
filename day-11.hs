#!/usr/bin/env cabal
{- cabal:
build-depends: base, vector, primitive, extra, ansi-terminal
-}
{-# language TupleSections, BlockArguments, MultiWayIf, ImplicitParams #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import System.Console.ANSI
import System.Time.Extra
import Data.Bool
import Data.Functor ((<&>))
import Control.Monad hiding (void)
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
--import qualified Data.Vector.Generic as GV

-- container
-- rows, cols, table as a map Int -> Char
type C = V.MVector (PrimState IO) Char
type CL = (Int, Int, C)

-----------------   generic aux stuff

(!) :: C -> Int -> IO Char
(!) = VM.unsafeRead

(<~) :: C -> Int -> Char -> IO ()
(<~) = VM.unsafeWrite

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-----------------  task-specific aux stuff

-- print the field
p ::
  (?rw :: Int, ?cl :: Int) =>
  C -> IO ()
p v = foldM_ s () is
  where
  s _ i = do
    st <- v ! i
    putChar st
    when ((i+1) `mod` ?cl == 0) $ putChar '\n'

-- 1-dimensional indices for a field of size rw x cl
is ::
  (?rw :: Int, ?cl :: Int) =>
  [Int]
is = [0 .. ?rw * ?cl - 1]

free, took, void :: Char
free = 'L'
took = '#'
void = '.'

-------------------- Part 1

animate :: Bool
animate = True

loop ::
  (?rw :: Int, ?cl :: Int) =>
  C -> C -> IO ()
loop ci co = do
  when animate do
    clearScreen
    p ci
    sleep 1
  changed <- iter ci co
  if changed
    then loop co ci
    else pure ()

iter ::
  (?rw :: Int, ?cl :: Int) =>
  C -> C -> IO Bool
iter ci co = foldM (upd ci co) False is

upd ::
  (?rw :: Int, ?cl :: Int) =>
  C -> C -> Bool -> Int -> IO Bool
upd ci co changed i = do
  c <- ci ! i
  if c == void then pure changed else do
    w <- weight ci i
    --putStrLn (show w)
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
  (?rw :: Int, ?cl :: Int) =>
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
  (ri, ci) = i `divMod` ?cl

-- Main

getInput :: IO CL
getInput = do
  cts <-  readFile "input/day-11.txt"
  let ls = lines cts
      cols = length $ ls !! 0
      rows = length ls
      cts' = filter (/= '\n') cts
  (rows, cols, ) <$> (V.unsafeThaw $ V.fromList cts')

main :: IO ()
main = do
  (rw, cl, inp1) <- getInput
  (_, _, inp2)   <- getInput
  res1 <-
    let ?rw = rw
        ?cl = cl in do
        loop inp1 inp2
        res1' <- foldM
          (\c i -> inp1 ! i <&> \s -> c + bool 0 1 (s==took))
          (0::Int)
          is
        pure res1'

      --res2 =
  print res1
