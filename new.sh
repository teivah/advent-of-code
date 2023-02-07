#!/bin/bash
# Example: sh new.sh rust 2015 7
cp -R template-$1 $2/day$3-$1
if [ "$1" = "go" ]; then
  cd $2/day$3-$1
  go mod init day$3-$1
  go get github.com/stretchr/testify
  go get github.com/teivah/advent-of-code@v0.0.24
  cd ../..
fi
curl --cookie "session=$ADVENT_OF_CODE_COOKIE" https://adventofcode.com/$2/day/$3/input -o $2/day$3-$1/input.txt
perl -i -pe 'chomp if eof' $2/day$3-$1/input.txt
idea $2/day$3-$1