# Advent of Code

The famous programming competition: [adventofcode.com][aoc].
Below are instructions on how to run the solutions.

[aoc]: https://adventofcode.com/

## 2023 (and somewhat 2019)

Make sure to clone `--recursive`ly because we now depend on a library
(`aoc-lib`) that lives here as a submodule.

```shell
cat Y2023/input/day-1.txt | cabal run y2023:day-1
```


## 2022

```shell
cd Y2022
cat input/day-1.txt | cabal run day-1.hs
```

## 2021

```shell
ghc Main.hs
Main DAY
```

## 2020

Most solutions are single-file and runnable with `cabal run <filename.hs>`
(or `cabal new-run ...` if you haven't upgraded to Cabal 3 yet).
