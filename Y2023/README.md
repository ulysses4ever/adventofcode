## Notes on AOC Y2023

### Day 3: Gear Ratios 

First search problem this year. I had to revive the memory of how to do a
tracing DFS using the `search-algorithms` package. Tracing here means that we
don't search we just need to explore the whole space. The way I do it is via the
monadic version of the DFS in the package: `dfsM`. I put the writer monad on top
of it and just record all that has been generated. Perhaps, I should create a
wrapper for it and add it to `aoc-lib`.
