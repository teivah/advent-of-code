package main

import (
	"context"
	"io"
	"math"
	"sort"
	"sync"

	aoc "github.com/teivah/advent-of-code"
	"golang.org/x/sync/errgroup"
)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	seeds := parseSeeds(groups[0][0])
	var maps []Map
	for i := 1; i < len(groups); i++ {
		maps = append(maps, parseMap(groups[i]))
	}

	lowest := math.MaxInt
	for _, v := range seeds {
		for _, m := range maps {
			if dst, contains := m.get(v); contains {
				v = dst
			}
		}
		lowest = min(lowest, v)
	}

	return lowest
}

func parseSeeds(line string) []int {
	line = aoc.Substring(line, ": ")
	del := aoc.NewDelimiter(line, " ")
	return del.GetInts()
}

type Range struct {
	// Included
	from int
	// Included
	to      int
	transfo int
}

type Map struct {
	ranges []Range
}

func (m Map) get(v int) (int, bool) {
	l := 0
	r := len(m.ranges) - 1
	for l <= r {
		mid := l + (r-l)/2
		rng := m.ranges[mid]
		if v > rng.to {
			l = mid + 1
		} else if v < rng.from {
			r = mid - 1
		} else {
			return v + rng.transfo, true
		}
	}

	return 0, false
}

func parseMap(lines []string) Map {
	var ranges []Range
	for i := 0; i < len(lines); i++ {
		if i == 0 {
			// Discard header
			continue
		}
		del := aoc.NewDelimiter(lines[i], " ")
		ints := del.GetInts()
		dstRange := ints[0]
		srcRange := ints[1]
		rangeLength := ints[2]

		ranges = append(ranges, Range{
			from:    srcRange,
			to:      srcRange + rangeLength - 1,
			transfo: dstRange - srcRange,
		})
	}
	sort.Slice(ranges, func(i, j int) bool {
		return ranges[i].from < ranges[j].from
	})
	return Map{ranges: ranges}
}

// See fs2 for the final version
func fs2NonFinal(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	seeds := parseSeedsRange(groups[0][0])
	var maps []Map
	for i := 1; i < len(groups); i++ {
		maps = append(maps, parseMap(groups[i]))
	}

	mu := sync.Mutex{}
	lowest := math.MaxInt
	wg, _ := errgroup.WithContext(context.Background())
	for _, seed := range seeds {
		seed := seed
		wg.Go(func() error {
			local := math.MaxInt
			for i := 0; i < seed[1]; i++ {
				local = min(local, transform(seed[0]+i, maps))
			}
			mu.Lock()
			lowest = min(lowest, local)
			mu.Unlock()
			return nil
		})
	}

	_ = wg.Wait()
	return lowest
}

func transform(v int, maps []Map) int {
	for _, m := range maps {
		if dst, contains := m.get(v); contains {
			v = dst
		}
	}
	return v
}

func parseSeedsRange(line string) [][2]int {
	line = aoc.Substring(line, ": ")
	del := aoc.NewDelimiter(line, " ")
	ints := del.GetInts()
	var res [][2]int
	for i := 0; i < len(ints); i += 2 {
		res = append(res, [2]int{ints[i], ints[i+1]})
	}
	return res
}

func fs2(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	seeds := parseSeedsRange(groups[0][0])
	sort.Slice(seeds, func(i, j int) bool {
		return seeds[i][0] < seeds[j][0]
	})
	var maps []Map
	for i := 1; i < len(groups); i++ {
		maps = append(maps, parseRevMap(groups[i]))
	}

	for loc := 0; loc < math.MaxInt; loc++ {
		v := transformRev(loc, maps)
		if isWithinSeedRange(v, seeds) {
			return loc
		}
	}

	return -1
}

func transformRev(v int, maps []Map) int {
	for i := len(maps) - 1; i >= 0; i-- {
		m := maps[i]
		if dst, contains := m.get(v); contains {
			v = dst
		}
	}
	return v
}

func parseRevMap(lines []string) Map {
	var ranges []Range
	for i := 0; i < len(lines); i++ {
		if i == 0 {
			// Discard header
			continue
		}
		del := aoc.NewDelimiter(lines[i], " ")
		ints := del.GetInts()
		dstRange := ints[0]
		srcRange := ints[1]
		rangeLength := ints[2]

		ranges = append(ranges, Range{
			from:    dstRange,
			to:      dstRange + rangeLength - 1,
			transfo: srcRange - dstRange,
		})
	}
	sort.Slice(ranges, func(i, j int) bool {
		return ranges[i].from < ranges[j].from
	})
	return Map{ranges: ranges}
}

func isWithinSeedRange(n int, seeds [][2]int) bool {
	l := 0
	r := len(seeds) - 1
	for l <= r {
		mid := l + (r-l)/2
		rng := seeds[mid]
		if n < rng[0] {
			r = mid - 1
		} else if n > rng[0]+rng[1]-1 {
			l = mid + 1
		} else {
			return true
		}
	}

	return false
}
