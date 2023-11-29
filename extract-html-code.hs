#!/usr/bin/env cabal
{- cabal:
build-depends: base
            , tagsoup
-}
import Text.HTML.TagSoup
main = interact $ \inp ->
  renderTags
  . take 1
  . drop 2
  $ dropWhile
    (not . isTagOpenName "pre")
    (parseTags inp)
