gen LANGUAGE YEAR DAY:
  #!/bin/bash
  cp -R template-{{LANGUAGE}} {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
  curl --cookie "session=$ADVENT_OF_CODE_COOKIE" https://adventofcode.com/{{YEAR}}/day/{{DAY}}/input -o {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
  perl -i -pe 'chomp if eof' {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
  if [ "{{LANGUAGE}}" = "go" ]; then
    cd {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
    go mod init day{{YEAR}}-{{DAY}}
    go get github.com/stretchr/testify
    go get github.com/teivah/advent-of-code@v0.0.27
    cd ../..
  fi
  idea {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
