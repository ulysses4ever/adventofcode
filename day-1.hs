main ::  IO ()
main = do
  cts <- readFile "input/day-1.txt"
  let xs = map read (lines cts) :: [Int]
      rs = [x*y*z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
  let res = head rs
  print res
  
