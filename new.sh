#!/bin/bash
# Example: sh new.sh rust 2015 7
cp -R template-$1 $2/day$3-$1
idea $2/day$3-$1