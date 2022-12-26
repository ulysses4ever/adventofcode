#!/usr/bin/env sh
set -e

SESSION=$(cat session)
YEAR=$(date +%Y)
#DAY=$(date +%-d)
if [ -z ${d+x} ]; then echo "ERROR: d (day) is unset"; exit 1; fi
DAY=${d}
curl --cookie "session=$SESSION" https://adventofcode.com/$YEAR/day/$DAY/input > input/day-$DAY.txt
