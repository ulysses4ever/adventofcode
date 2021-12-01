#!/usr/bin/env cabal
{- cabal:
build-depends: base, mtl, microlens-platform
-}
{-# language TemplateHaskell, BlockArguments, MultiWayIf, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Control.Applicative
import Control.Monad.State
import Lens.Micro.Platform

type Str = String
type Acc = Int
type PC = Int

data St = S {
  _ac   :: Acc,
  _pc   :: PC,
  _pcs  :: [PC],  -- visited instructions
  _fork :: Bool,  -- have we forked yet
  _trc  :: [Str], -- trace executed instructions
  _prog :: [Str]} -- our program
  deriving Show

$(makeLenses ''St)

type Eval = StateT St [] ()

recordPc :: Eval
recordPc = modify \s -> over pcs (view pc s :) s

incPc :: Eval
incPc = modify $ over pc (+1)

trace' :: Str -> Int -> Eval
trace' c n = modify $ over trc ((c ++ show n) :)
trace'' :: Str -> Eval
trace'' c = modify $ over trc (c :)

acc, jmp, nop :: Int -> Eval
acc n = do
  trace' "acc " n
  modify $ over ac (+ n)
  incPc
jmp n = do
  trace' "jmp " n
  modify $ over pc (+ n)
nop n = do
  trace' "nop " n
  incPc

mayFork :: Eval -> Eval
mayFork cmd = do
  S{..} <- get
  unless _fork do
    put S{_fork = True, ..}
    trace'' "FORK"
    cmd

eval :: Str -> Eval
eval s = let
  (cs, a) = case words s of
    [cs', as] -> (cs', (read . dropWhile (== '+')) $ as)
    _         -> error $ "Can't parse command: " ++ s
  in do
  recordPc
  case cs of
    "acc" -> acc a
    "jmp" -> jmp a <|> mayFork (nop a)
    "nop" -> nop a <|> mayFork (jmp a)
    _     -> error $ "unknown command: " ++ cs

evalLoop :: Eval
evalLoop = do
  S{..} <- get

  if | _pc `elem` _pcs     ->
        trace'' "DIE" >>
          mzero -- use `pure ()` instead to collect all traces

     | _pc >= length _prog ->
        trace'' "WIN" >> pure ()

     | otherwise           -> do
          -- trace'' "LOOP "
          eval $ _prog !! _pc
          evalLoop

main :: IO ()
main = do
  inp <- readFile "input/day-8.txt"
  let
    res2 = view (_2 . ac) . head .
      runStateT evalLoop $
      (S 0 1 [] False [] $ "" : lines inp)
  print res2
