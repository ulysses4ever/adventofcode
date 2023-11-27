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
#
set -e

: "${SESSION:=./session}"
: "${YEAR:=$(date +%Y)}"
: "${DAY:=$(date +%-d)}"
: "${OUT:=$YEAR/input/day-$DAY.txt}"
SESSIONKEY=$(cat "$SESSION")
# echo "DAY=$DAY, YEAR=$YEAR"
curl --cookie "session=$SESSIONKEY" https://adventofcode.com/"$YEAR"/day/"$DAY"/input > "$OUT"
