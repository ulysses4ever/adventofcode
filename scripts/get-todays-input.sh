#!/usr/bin/env bash
#
# Get AoC input.
# Parameters, passed via env vars, all optional:
# - SESSION -- filename with a cookie to access the AoC website.
#              Default: ./session
# - YEAR -- year of the challenge. Default: current year.
# - DAY -- day of the challenge. Default: current day.
# - OUT -- where to route the output. Default: $YEAR/input/day-$DAY.txt.
#          May be useful to set it to '/deb/stdout'.
# - SAMPLE -- try to extract sample input from the text of the problem. Default: unset,
#             which means False. If set, means True and overrides the main mode.
#             Doesn't need a session file. Print result to stdout.
#             This mode requires Haskell toolchain (GHC+Cabal) to parse HTML.
#
set -e

: "${YEAR:=$(date +%Y)}"
: "${DAY:=$(date +%-d)}"

if [ -v SAMPLE ]; then
  ME="$(realpath "${BASH_SOURCE[-1]}")"
  DIR="$(dirname "$ME")"
  wget -q -O - https://adventofcode.com/"$YEAR"/day/"$DAY" | $DIR/extract-html-code.hs
  exit 0;
fi

: "${OUT:=$YEAR/input/day-$DAY.txt}"
: "${SESSION:=./session}"
SESSIONKEY=$(cat "$SESSION")
# echo "DAY=$DAY, YEAR=$YEAR"
curl --cookie "session=$SESSIONKEY" https://adventofcode.com/"$YEAR"/day/"$DAY"/input > "$OUT"
