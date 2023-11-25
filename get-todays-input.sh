#!/usr/bin/env bash
#
# Get AoC input.
# Parameters (passed via env vars):
# - SESSION -- filename with a cookie to access the AoC website.
#              Default: ./session
# - YEAR -- year of the challenge. Default: current year.
# - DAY -- day of the challenge. Default: current day.
#
set -e

: "${SESSION:=./session}"
: "${YEAR:=$(date +%Y)}"
: "${DAY:=$(date +%-d)}"
SESSIONKEY=$(cat "$SESSION")
# echo "DAY=$DAY, YEAR=$YEAR"
curl --cookie "session=$SESSIONKEY" https://adventofcode.com/"$YEAR"/day/"$DAY"/input
