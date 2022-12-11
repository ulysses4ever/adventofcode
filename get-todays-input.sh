#!/usr/bin/env sh
set -e

SESSION=$(cat session)
YEAR=$(date +%Y)
DAY=$(date +%-d)
curl --cookie "session=$SESSION" https://adventofcode.com/$YEAR/day/$DAY/input > input/day-$DAY.txt
