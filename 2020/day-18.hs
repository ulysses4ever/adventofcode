#!/usr/bin/env cabal
{- cabal:
build-depends: base, megaparsec, parser-combinators
-}
{-# OPTIONS_GHC -Wall -O1 #-}
module Main where

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Maybe

type Str = String
type Parser = Parsec Void Str
type OpTable = [[Operator Parser E]]

data E -- Expr
  = I Int
  | S E E
  | P E E
  deriving (Show)

-- Parser

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Str -> Parser Str
symbol = L.symbol space

pInt :: Parser E
pInt = I <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: OpTable -> Parser E
pTerm optable = choice
  [ parens (pExpr optable)
  , pInt
  ]

pExpr :: OpTable -> Parser E
pExpr optable = makeExprParser (pTerm optable) optable

opTable1 :: OpTable
opTable1 = [ [
  binary "*" P,
  binary "+" S] ]

opTable2 :: OpTable
opTable2 = [ [
  binary "+" S], [
  binary "*" P] ]

binary :: Str -> (E -> E -> E) -> Operator Parser E
binary name f = InfixL  (f <$ symbol name)

-- Eval

eval :: E -> Int
eval (I n) = n
eval (S e1 e2) = eval e1 + eval e2
eval (P e1 e2) = eval e1 * eval e2

-- Main

getInp :: IO [String]
getInp =
  lines <$> readFile "input/day-18.txt"

main :: IO ()
main = do
  inp <- getInp
  let runParse optable = map (parseMaybe $ pExpr optable)
      mes1 = runParse opTable1 inp
      mes2 = runParse opTable2 inp
      getSum = sum . map (eval . fromJust)
      res1 = getSum mes1
      res2 = getSum mes2
  print res1
  print res2
