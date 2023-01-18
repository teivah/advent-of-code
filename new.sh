#!/bin/bash
# Example: sh new.sh rust 2015 7
cp -R template-$1 $2/day$3-$1
if [ "$1" = "go" ]; then
  cd $2/day$3-$1
  go mod init day$3-$1
  go get github.com/stretchr/testify
  go get github.com/teivah/advent-of-code@main
  cd ../..
fi
idea $2/day$3-$1