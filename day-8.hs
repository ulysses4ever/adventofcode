#!/usr/bin/env cabal
{- cabal:
build-depends: base, mtl, microlens-platform, pretty-simple
-}
{-# language LambdaCase, TemplateHaskell, BlockArguments #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Control.Monad.Except
import Control.Monad.State
import Lens.Micro.Platform
--import Text.Pretty.Simple
import Data.Functor.Identity

type Str = String
type Acc = Int
type PC = Int

data St = S {
  _ac :: Acc,
  _pc :: PC,
  _pcs :: [PC], -- ^ visited
  _prog :: [Str]}
  deriving Show
$(makeLenses ''St)

type Eval = State St ()

incPc :: Eval
incPc = modify
  \(S ac' pc' pcs' prog') -> S ac' (pc'+1) (pc':pcs') prog'

acc, jmp, nop :: Int -> Eval
acc n = modify $ over ac (+ n)
jmp n = modify $ over pc (+ n)
nop _ = pure ()

control :: [Str]
control = ["jmp"]

eval :: Str -> Eval
eval s = let
  (cs, a) = case words s of
    [cs', as] -> (cs', (read . dropWhile (== '+')) $ as)
    _         -> error $ "Can't parse command: " ++ s
  in
  a & case cs of -- I wish I had the real eval!
    "acc" -> acc
    "jmp" -> jmp
    "nop" -> nop
    _     -> error $ "unknown command: " ++ cs
  >> unless (cs `elem` control) incPc

evalLoop :: Eval
evalLoop = do
  (S _ac' pc' pcs' prog') <- get
  unless (pc' `elem` pcs' || pc' >= length prog') do
    eval $ prog' !! pc'
    evalLoop

main :: IO ()
main = do
  inp <- readFile "input/day-8.txt"
  let
    res1 = view (_2 . ac) . runIdentity . runStateT
      evalLoop $ (S 0 0 [] $ lines inp)
    res2 = 2 :: Int
  print res1
  print res2
