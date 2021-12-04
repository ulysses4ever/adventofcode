module Y2021 where

import Y2021.Day1
import Y2021.Day2
import Y2021.Day3
import Y2021.Day4

solutions =
  [ undefined -- all hail 1-based indexing
  , (Y2021.Day1.solve1, Y2021.Day1.solve2)
  , (Y2021.Day2.solve 1, Y2021.Day2.solve 2)
  , (Y2021.Day3.solve 1, Y2021.Day3.solve 2)
  , (Y2021.Day4.solve 1, Y2021.Day4.solve 2)
  ]
