#!/usr/bin/env sh
set -e

SESSION=$(cat session)
: "${YEAR:=$(date +%Y)}"
: "${DAY:=$(date +%-d)}"
# echo "DAY=$DAY, YEAR=$YEAR"
curl --cookie "session=$SESSION" https://adventofcode.com/$YEAR/day/$DAY/input
