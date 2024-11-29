## Notes on AOC Y2023

### Day 11: [NOT SOLVED] Cosmic Exxpansion

It's more or less Dijkstra (they literally say "shortest paths" in the text) but
I'm very slow at this (even with `search-algorithms`) and I still don't have
generic parsing for 2D structures ("maps").

### Day 10: [NOT SOLVED] Pipes

People say it's the "flood fill" algorithm but I tend to just call DFS in such
cases and "trace" what verticies we saw. See tracing DFS in Day 3.

### Day 9: Extrapolate Histories

This was very dumb problem which took me several hours just because my parser is
broken: it doesn't parser signed numbers properly but doesn't fail either.

### Day 8: [NOT SOLVED in 2023, solved in 2024]

Some graph traversal (but people say that you can also use number theory and
CRT, I think). I skipped for time reasons but brute force looked simple (parsing
being the most annoying part).

``` text
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
...
```

**2024 update** I solved it with a hint from Reddit for Part 2.

For Part 1 I tried a fancy idea (search source from sink instead of the other way
round) but it turned out to not work (I guess, it's ambiguous). Then simple
interpreter worked for Part 1. And for Part 2 you had to search from several
sources and wait while you hit the sink simultaneously. The hint I needed:
instead of actually searching "in parallel", search sequentially and then apply
LCM to the results.

Parsing with my library `aoc-lib` (the `Input` type class) is a breeze
(almost... still some noise due to `coerce`).

Added `iterateWhile` to `aoc-lib` to factor out simple iteration algorithm with
a state that waits for a predicate to shoot. It's annoying toi encode the state
with tuples, though. But perhaps some of the immutable "context" parameters can
be just captures instead of actual parameters.


### Day 7: Poker

Haskell's deriving shine in generating boilerplate for defining order.
Unfortunately, I can't easily change it when they adjusted order in the second
part. I tried to use CPP to easily switch b/w the too possible order of card
values. That was pretty ugly and also only half-working.

### Day 6: Boat Races

One of those tasks which you can solve with pen and paper if you're smart but
I'm not so I did a simple brute force, which went just fine. They hid a
quadratic inequality in there: `x*(time-x) > old_record`.

### Day 5: Seeding the garden

Interval arithmetics strikes again. I didn't have time to implement it and went
for brute force (enumerating interval elements), which is pretty said. It took
13 minutes on the server!

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
