## Notes on AOC Y2023

### Day 4: Scratchcards

Relatively easy Monday problem. Splitting lines on a (single) `|` is something
I with I could do with a library function.

Part 2, where you need to count the number of copies of your scratchcards 
was rather annoying to solve because it required a non-trivial fold:

``` haskell
copies = fst $ foldl' f ([], repeat 1) winCounts
  where
    f (res, c:cs) w = (c:res,
                      zipWith (+) cs (replicate w c ++ repeat 0))
```

I wish a `zipWith` that leaves the tail of a longer tail intact was readily
available (wouldn't need that `repeat 0` in that case).

### Day 3: Gear Ratios 

First search problem this year. I had to revive the memory of how to do a
tracing DFS using the `search-algorithms` package. Tracing here means that we
don't search we just need to explore the whole space. The way I do it is via the
monadic version of the DFS in the package: `dfsM`. I put the writer monad on top
of it and just record all that has been generated. Perhaps, I should create a
wrapper for it and add it to `aoc-lib`.
