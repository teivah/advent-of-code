gen LANGUAGE YEAR DAY:
  #!/bin/bash
  [[ -s "$GVM_ROOT/scripts/gvm" ]] && source "$GVM_ROOT/scripts/gvm"
  gvm use go1.21.4
  cp -R templates/{{LANGUAGE}} {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
  curl --cookie "session=$ADVENT_OF_CODE_COOKIE" https://adventofcode.com/{{YEAR}}/day/{{DAY}}/input -o {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
  perl -i -pe 'chomp if eof' {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
  if [ "{{LANGUAGE}}" = "go" ]; then
    sed -i '' '49i\
  * [Day {{DAY}}](https://adventofcode.com/{{YEAR}}/day/{{DAY}}): [Go]({{YEAR}}/day{{DAY}}-go/main.go)\
  ' README.md

    cd {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
    go mod init day{{YEAR}}-{{DAY}}
    go mod tidy
    go get github.com/stretchr/testify
    go get github.com/teivah/advent-of-code@v1.1.1
    go get golang.org/x/exp

    # Temporary workaround as the Go version is generated with 3 digits for some reason
    sed -i -e 's/go 1.21.4/go 1.21/' go.mod
    rm go.mod-e

    cd ../..
  fi

  if [ "{{LANGUAGE}}" = "rust" ]; then
    sed -i '' '49i\
  * [Day {{DAY}}](https://adventofcode.com/{{YEAR}}/day/{{DAY}}): [Rust]({{YEAR}}/day{{DAY}}-rust/src/lib.rs)\
  ' README.md
  fi

  if [ "{{LANGUAGE}}" = "python" ]; then
    sed -i '' '49i\
  * [Day {{DAY}}](https://adventofcode.com/{{YEAR}}/day/{{DAY}}): [Python]({{YEAR}}/day{{DAY}}-python/main.py)\
  ' README.md
  fi


  idea {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
