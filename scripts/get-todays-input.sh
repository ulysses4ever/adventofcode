#!/usr/bin/env bash
#
# Get AoC input. If using the default $OUT, run from the root of the repo.

# Parameters, passed via env vars, all optional:
# - SESSION -- filename with a cookie to access the AoC website.
#              Default: ../session
# - YEAR -- year of the challenge. Default: current year.
# - DAY -- day of the challenge. Default: current day.
# - OUT -- where to route the output. Default: $YEAR/input/day-$DAY.txt.
#          May be useful to set it to '/dev/stdout'.
# - SAMPLE -- try to extract sample input from the text of the problem. Default: unset,
#             which means False. If set, means True and overrides the main mode.
#             Doesn't need a session file. Print result to stdout.
#             This mode requires Haskell toolchain (GHC+Cabal) to parse HTML.
#
set -e

ME="$(realpath "${BASH_SOURCE[-1]}")"
DIR="$(dirname "$ME")"

: "${YEAR:=$(date +%Y)}"
: "${DAY:=$(date +%-d)}"

if [ -v SAMPLE ]; then
  wget -q -O - https://adventofcode.com/"$YEAR"/day/"$DAY" | $DIR/extract-html-code.hs
  exit 0;
fi

: "${OUT:=Y"$YEAR"/input/day-$DAY.txt}"
: "${SESSION:="$DIR"/../session}"
SESSIONKEY=$(cat "$SESSION")
# echo "DAY=$DAY, YEAR=$YEAR"
curl --cookie "session=$SESSIONKEY" https://adventofcode.com/"$YEAR"/day/"$DAY"/input > "$OUT"
