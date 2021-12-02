module Main where

import System.Environment

import Y2021 (solutions)

defaultYear = "2021"

allSolutions "2021" = Y2021.solutions

main = do
  args <- getArgs
  (y, d) <- case args of
    [y, d] -> return (y, d)
    [d]    -> return (defaultYear, d)
    _      -> error "Provide one (day) or two (year, day) arguments"
  let (s1, s2) = (allSolutions y) !! (read d)
  input <- readFile $ "Y" ++ y ++ "/input/day-" ++ d ++ ".txt" 
  s1 input
  s2 input

