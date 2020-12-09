#!/usr/bin/env cabal
{- cabal:
build-depends: base, mtl, microlens-platform, pretty-simple
-}
{-# language LambdaCase, TemplateHaskell, BlockArguments, MultiWayIf, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Lens.Micro.Platform
--import Text.Pretty.Simple

type Str = String
type Acc = Int
type PC = Int

data St = S {
  _ac   :: Acc,
  _pc   :: PC,
  _pcs  :: [PC], -- ^ visited
  _fork :: Bool,
  _prog :: [Str]}
  deriving Show
$(makeLenses ''St)

type Eval = StateT St [] ()

incPc :: Eval
incPc = modify
  \(S ac' pc'     pcs'       fork' prog') ->
    S ac' (pc'+1) (pc':pcs') fork' prog'

acc, jmp, nop :: Int -> Eval
acc n = modify $ over ac (+ n)
jmp n = modify $ over pc (+ n)
nop _ = pure ()

control :: [Str]
control = ["jmp"]

mayFork :: Eval -> Eval
mayFork cmd = do
  S{..} <- get
  unless _fork do
    put S{_fork = True, ..}
    cmd

eval :: Str -> Eval
eval s = let
  (cs, a) = case words s of
    [cs', as] -> (cs', (read . dropWhile (== '+')) $ as)
    _         -> error $ "Can't parse command: " ++ s
  in
  case cs of -- I wish I had the real eval!
    "acc" -> acc a
    "jmp" -> jmp a <|> mayFork (nop a)
    "nop" -> nop a <|> mayFork (jmp a)
    _     -> error $ "unknown command: " ++ cs
  >> unless (cs `elem` control) incPc

evalLoop :: Eval
evalLoop = do
  S{..} <- get
  if | _pc `elem` _pcs     -> mzero
     | _pc >= length _prog -> pure ()
     | otherwise           -> do
        eval $ _prog !! _pc
        evalLoop

main :: IO ()
main = do
  inp <- readFile "input/day-8.txt"
  let
    res1 = view (_2 . ac) . head . runStateT
      evalLoop $ (S 0 0 [] False $ lines inp)
    res2 = 2 :: Int
  print res1
  print res2
