#!/usr/bin/env cabal
{- cabal:
build-depends: base
            , tagsoup
-}

{-

  Extract the third <pre>...</pre> block from an HTML input.

  > curl -s https://adventofcode.com/2025/day/1 | ./scripts/extract-html-code.hs

  L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82

-}

import Text.HTML.TagSoup
main = interact $ \inp ->
  renderTags
  . take 1
  . drop 2
  $ dropWhile
    (not . isTagOpenName "pre")
    (parseTags inp)
