#!/usr/bin/env cabal
{- cabal:
build-depends: base
-}
main ::  IO ()
main = do
  cts <- readFile "input/day-1.txt"
  let xs = map read (lines cts) :: [Int]
      rs = [x*y   | x <- xs, y <- xs, x + y == 2020]
      res1 = head rs
      rs2 = [x*y*z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
      res2 = head rs2
  print res1
  print res2
