package day15_go

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
)

type item int

const (
	sensorItem item = iota
	beaconItem
	noBeaconItem
	emptyItem
)

type pos struct {
	x int
	y int
}

type sensor struct {
	position pos
	beacon   pos
}

func fn1(input io.Reader, nRow int) (int, error) {
	scanner := bufio.NewScanner(input)
	minX := math.MaxInt
	minY := math.MaxInt
	maxX := math.MinInt
	maxY := math.MinInt

	sensors := make([]sensor, 0)
	for scanner.Scan() {
		s := scanner.Text()

		start := 12
		end := strings.Index(s, ",")
		sensorX, err := strconv.Atoi(s[start:end])
		if err != nil {
			return 0, err
		}

		start = strings.Index(s[end:], "=") + 1 + end
		end = strings.Index(s[start:], ":") + start
		sensorY, err := strconv.Atoi(s[start:end])
		if err != nil {
			return 0, err
		}

		start = strings.Index(s[end:], "=") + 1 + end
		end = strings.Index(s[start:], ",") + start
		beaconX, err := strconv.Atoi(s[start:end])
		if err != nil {
			return 0, err
		}

		start = strings.Index(s[end:], "=") + 1 + end
		beaconY, err := strconv.Atoi(s[start:])
		if err != nil {
			return 0, err
		}

		sensors = append(
			sensors, sensor{
				position: pos{sensorX, sensorY},
				beacon:   pos{beaconX, beaconY},
			},
		)

		if sensorX < minX {
			minX = sensorX
		}
		if sensorY < minY {
			minY = sensorY
		}
		if beaconX < minX {
			minX = beaconX
		}
		if beaconY < minY {
			minY = beaconY
		}
		if sensorX > maxX {
			maxX = sensorX
		}
		if sensorY > maxY {
			maxY = sensorY
		}
		if beaconX > maxX {
			maxX = beaconX
		}
		if beaconY > maxY {
			maxY = beaconY
		}
	}

	rows := make([]row, 0, maxY-minY+1)
	for y := minY; y <= maxY; y++ {
		rows = append(
			rows, row{
				beacons: make([]pos, 0),
				sensors: make([]pos, 0),
			},
		)
	}
	g := graph{rows: rows, minX: minX, minY: minY, maxX: maxX, maxY: maxY}

	for _, sensor := range sensors {
		y := g.row(sensor.position.y)
		g.rows[y].sensors = append(g.rows[y].sensors, sensor.position)

		y = g.row(sensor.beacon.y)
		g.rows[y].beacons = append(g.rows[y].sensors, sensor.beacon)
	}

	g.calcDistances(sensors)

	for i, sensor := range sensors {
		g.expand(sensor.position.x, sensor.position.y, i)
	}

	g.print()
	return g.countNoBeacon(nRow), nil
}

type graph struct {
	rows     []row
	minX     int
	maxX     int
	minY     int
	maxY     int
	distance []int
}

func (g *graph) calcDistances(sensors []sensor) {
	g.distance = make([]int, len(sensors))
	for i, sensor := range sensors {
		g.distance[i] = distance(sensor.position, sensor.beacon)
	}
}

func distance(from, to pos) int {
	return abs(from.x-to.x) + abs(from.y-to.y)
}

func abs(i int) int {
	if i > 0 {
		return i
	}
	return -i
}

func (g *graph) row(y int) int {
	return y - g.minY
}

func (g *graph) expand(x, y int, i int) {
	dst := g.distance[i]

	l := x - dst
	r := x + dst
	rowY := y
	for l <= r {
		v := g.row(rowY)
		if v < 0 || v >= len(g.rows) {
			break
		}
		row := g.rows[v]
		row.addInterval(l, r, noBeaconItem)
		g.rows[v] = row
		rowY++
		l++
		r--
	}

	l = x - dst + 1
	r = x + dst - 1
	rowY = y - 1
	for l <= r {
		v := g.row(rowY)
		if v < 0 || v >= len(g.rows) {
			break
		}
		row := g.rows[v]
		row.addInterval(l, r, noBeaconItem)
		g.rows[v] = row
		rowY--
		l++
		r--
	}
}

func (g *graph) countNoBeacon(y int) int {
	y = g.row(y)
	r := g.rows[y]
	count := 0
	for _, interval := range r.intervals {
		if interval.item == noBeaconItem {
			count += interval.to - interval.from + 1

			for _, beacon := range r.beacons {
				if beacon.x >= interval.from && beacon.x <= interval.to {
					count--
				}
			}

			for _, sensor := range r.sensors {
				if sensor.x >= interval.from && sensor.x <= interval.to {
					count--
				}
			}
		}
	}
	return count
}

func (g *graph) expandCount(x, y, count int) bool {
	if count == 0 {
		return false
	}

	y = g.row(y)

	if y < 0 || y >= len(g.rows) {
		return false
	}

	// TODO improve
	itm := g.rows[y].get(x)
	if itm == beaconItem {
		return true
	}

	res := false
	res = res || g.expandCount(x-1, y, count-1)
	res = res || g.expandCount(x+1, y, count-1)
	res = res || g.expandCount(x, y-1, count-1)
	res = res || g.expandCount(x, y+1, count-1)
	return res
}

func (g *graph) print() {
	for i, r := range g.rows {
		_ = i
		for x := g.minX; x <= g.maxX; x++ {
			item := r.get(x)
			switch item {
			case sensorItem:
				fmt.Print("S")
			case beaconItem:
				fmt.Print("B")
			case noBeaconItem:
				fmt.Print("#")
			case emptyItem:
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
	fmt.Println()
	fmt.Println()
}

func (g *graph) write() {
	f, err := os.Create("/tmp/dat2")
	if err != nil {
		panic(err)
	}
	defer f.Close()
	w := bufio.NewWriter(f)

	for i, r := range g.rows {
		_ = i
		for x := g.minX; x <= g.maxX; x++ {
			item := r.get(x)
			switch item {
			case sensorItem:
				w.WriteString("S")
			case beaconItem:
				w.WriteString("B")
			case noBeaconItem:
				w.WriteString("#")
			case emptyItem:
				w.WriteString(".")
			}
		}
		w.WriteString("\n")
	}
	w.Flush()
}

type interval struct {
	from int
	to   int
	item item
}

type row struct {
	intervals []interval
	beacons   []pos
	sensors   []pos
}

func (r *row) get(x int) item {
	for _, sensor := range r.sensors {
		if sensor.x == x {
			return sensorItem
		}
	}

	for _, beacon := range r.beacons {
		if beacon.x == x {
			return beaconItem
		}
	}

	for _, interval := range r.intervals {
		if x >= interval.from && x <= interval.to {
			return interval.item
		}
	}

	return emptyItem
}

func (r *row) addInterval(from, to int, item item) {
	r.intervals = append(r.intervals, interval{from, to, item})
	sort.Slice(
		r.intervals, func(i, j int) bool {
			a := r.intervals[i]
			b := r.intervals[j]
			if a.from < b.from {
				return true
			}
			if b.from < a.from {
				return false
			}
			if a.to < b.to {
				return true
			}
			return false
		},
	)

	res := make([]interval, 0, len(r.intervals))
	previous := r.intervals[0]
	for i := 1; i < len(r.intervals); i++ {
		next := r.intervals[i]
		if (previous.from <= next.from && next.from <= previous.to) ||
			(next.from <= previous.from && previous.from <= next.to) {
			previous = interval{
				from: min(previous.from, next.from),
				to:   max(previous.to, next.to),
				item: item,
			}
		} else {
			res = append(res, previous)
			previous = next
		}
	}
	r.intervals = append(res, previous)
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func fn2(input io.Reader, max int) (int, error) {
	scanner := bufio.NewScanner(input)
	minX := math.MaxInt
	minY := math.MaxInt
	maxX := math.MinInt
	maxY := math.MinInt

	sensors := make([]sensor, 0)
	for scanner.Scan() {
		s := scanner.Text()

		start := 12
		end := strings.Index(s, ",")
		sensorX, err := strconv.Atoi(s[start:end])
		if err != nil {
			return 0, err
		}

		start = strings.Index(s[end:], "=") + 1 + end
		end = strings.Index(s[start:], ":") + start
		sensorY, err := strconv.Atoi(s[start:end])
		if err != nil {
			return 0, err
		}

		start = strings.Index(s[end:], "=") + 1 + end
		end = strings.Index(s[start:], ",") + start
		beaconX, err := strconv.Atoi(s[start:end])
		if err != nil {
			return 0, err
		}

		start = strings.Index(s[end:], "=") + 1 + end
		beaconY, err := strconv.Atoi(s[start:])
		if err != nil {
			return 0, err
		}

		sensors = append(
			sensors, sensor{
				position: pos{sensorX, sensorY},
				beacon:   pos{beaconX, beaconY},
			},
		)

		if sensorX < minX {
			minX = sensorX
		}
		if sensorY < minY {
			minY = sensorY
		}
		if beaconX < minX {
			minX = beaconX
		}
		if beaconY < minY {
			minY = beaconY
		}
		if sensorX > maxX {
			maxX = sensorX
		}
		if sensorY > maxY {
			maxY = sensorY
		}
		if beaconX > maxX {
			maxX = beaconX
		}
		if beaconY > maxY {
			maxY = beaconY
		}
	}

	rows := make([]row, 0, maxY-minY+1)
	for y := minY; y <= maxY; y++ {
		rows = append(
			rows, row{
				beacons: make([]pos, 0),
				sensors: make([]pos, 0),
			},
		)
	}
	g := graph{rows: rows, minX: minX, minY: minY, maxX: maxX, maxY: maxY}

	for _, sensor := range sensors {
		y := g.row(sensor.position.y)
		g.rows[y].sensors = append(g.rows[y].sensors, sensor.position)
		g.rows[y].addInterval(sensor.position.x, sensor.position.x, noBeaconItem)

		y = g.row(sensor.beacon.y)
		g.rows[y].beacons = append(g.rows[y].sensors, sensor.beacon)
		g.rows[y].addInterval(sensor.beacon.x, sensor.beacon.x, noBeaconItem)
	}

	g.calcDistances(sensors)

	for i, sensor := range sensors {
		g.expand(sensor.position.x, sensor.position.y, i)
	}

	for i := 0; i < max; i++ {
		y := g.row(i)
		r := g.rows[y]
		if len(r.intervals) == 0 || len(r.intervals) == 1 {
			continue
		}
		previous := r.intervals[0]
		for j := 1; j < len(r.intervals); j++ {
			next := r.intervals[j]
			if previous.to+2 == next.from {
				return (previous.to+1)*4000000 + i, nil
			}
		}
	}

	return 0, nil
}
