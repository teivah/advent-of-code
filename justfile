gen LANGUAGE YEAR DAY:
  #!/bin/bash
  if [ "{{LANGUAGE}}" = "go" ]; then
    [[ -s "$GVM_ROOT/scripts/gvm" ]] && source "$GVM_ROOT/scripts/gvm"
    gvm use go1.23.3
    cp -R templates/{{LANGUAGE}} {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
    curl --cookie "session=$ADVENT_OF_CODE_COOKIE" https://adventofcode.com/{{YEAR}}/day/{{DAY}}/input -o {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
    perl -i -pe 'chomp if eof' {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
    sed -i '' '55i\
  * [Day {{DAY}}](https://adventofcode.com/{{YEAR}}/day/{{DAY}}): [Go]({{YEAR}}/day{{DAY}}-go/main.go)\
  ' README.md

    cd {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
    go mod init day{{YEAR}}-{{DAY}}
    go mod tidy
    go get github.com/stretchr/testify
    go get github.com/teivah/advent-of-code
    go get golang.org/x/exp

    cd ../..
  fi

  if [ "{{LANGUAGE}}" = "rust" ]; then
    cp -R templates/{{LANGUAGE}} {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
    curl --cookie "session=$ADVENT_OF_CODE_COOKIE" https://adventofcode.com/{{YEAR}}/day/{{DAY}}/input -o {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
    perl -i -pe 'chomp if eof' {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
    sed -i '' '55i\
  * [Day {{DAY}}](https://adventofcode.com/{{YEAR}}/day/{{DAY}}): [Rust]({{YEAR}}/day{{DAY}}-rust/src/lib.rs)\
  ' README.md
  fi

  if [ "{{LANGUAGE}}" = "python" ]; then
    cp -R templates/{{LANGUAGE}} {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
    curl --cookie "session=$ADVENT_OF_CODE_COOKIE" https://adventofcode.com/{{YEAR}}/day/{{DAY}}/input -o {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
    perl -i -pe 'chomp if eof' {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
    sed -i '' '55i\
  * [Day {{DAY}}](https://adventofcode.com/{{YEAR}}/day/{{DAY}}): [Python]({{YEAR}}/day{{DAY}}-python/main.py)\
  ' README.md
  fi

  if [ "{{LANGUAGE}}" = "haskell" ]; then
    cd {{YEAR}}
    stack new day{{DAY}}-haskell
    cd ..
    cp templates/haskell/Justfile {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
    sed -i -e 's/X/{{DAY}}/' {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/Justfile
    rm {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/Justfile-e
    rm {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/src/Lib.hs
    cp templates/haskell/Lib/* {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/src
    rm {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/app/Main.hs
    cp templates/haskell/Main.hs {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/app
    cd {{YEAR}}/day{{DAY}}-{{LANGUAGE}}
    stack setup
    cd ../..
    curl --cookie "session=$ADVENT_OF_CODE_COOKIE" https://adventofcode.com/{{YEAR}}/day/{{DAY}}/input -o {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
    perl -i -pe 'chomp if eof' {{YEAR}}/day{{DAY}}-{{LANGUAGE}}/input.txt
  fi

  idea {{YEAR}}/day{{DAY}}-{{LANGUAGE}}

stats:
  go run cmd/stats.go
  echo "Go lines: $(find . -name '*.go' -exec cat {} + | wc -l)"
  echo "Go lines without tests: $(find . -name '*.go' ! -name '*_test.go' -exec cat {} + | wc -l)"
  echo "Rust lines: $(find . -name '*.rs' -exec cat {} + | wc -l)"
  echo "Haskell lines: $(find . -name '*.hs' -exec cat {} + | wc -l)"
  echo "Python lines: $(find . -name '*.py' -exec cat {} + | wc -l)"
