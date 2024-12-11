#!/usr/bin/env sh

#
# Creates a new day solution template. Run from the root of the project.
#
# Parameters (all optional):
# - DAY -- day index defaulting to today
# - YEAR -- year defaulting to this year
# - SUFFIX -- suffix for the filename with solution. Don't forget the leading `-`.
#
# Assumes:
# - Y$YEAR/day-N.hs exists with a day solution template
#
# Effects:
# - Creates Y$YEAR/day-$DAY.hs from the template (see Assumes)
# - Adds an executable component day-$DAY to the cabal file of the year
#

: "${YEAR:=$(date +%Y)}"
: "${DAY:=$(date +%-d)}"
# : "${SUFFIX:=}"
DAYFILE="day-$DAY$SUFFIX.hs"

cp "Y$YEAR/day-N.hs" "Y$YEAR/$DAYFILE"
echo -n "
executable day-$DAY
    import:           base
    main-is:          $DAYFILE
" >> Y$YEAR/y$YEAR.cabal

wget "https://adventofcode.com/$YEAR/day/$DAY" -O - | ./scripts/extract-html-code.hs > Y$YEAR/input/day-$DAY-small.txt
