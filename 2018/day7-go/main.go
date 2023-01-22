package main

import (
	"bufio"
	"io"
	"sort"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) string {
	scanner := bufio.NewScanner(input)

	befores := make(map[string][]string)
	for scanner.Scan() {
		line := scanner.Text()
		del := lib.NewDelimiter(line, " ")
		from := del.GetString(1)
		to := del.GetString(7)
		befores[from] = append(befores[from], to)
	}

	inDegree := make(map[string]int)
	for k, values := range befores {
		inDegree[k] = 0
		for _, v := range values {
			inDegree[v] = 0
		}
	}

	for _, values := range befores {
		for _, v := range values {
			inDegree[v]++
		}
	}

	var q []string
	for k, v := range inDegree {
		if v == 0 {
			q = append(q, k)
		}
	}

	res := ""
	for len(q) != 0 {
		sort.Strings(q)

		v := q[0]
		q = q[1:]
		res += v

		for _, values := range befores[v] {
			inDegree[values]--
			if inDegree[values] == 0 {
				q = append(q, values)
			}
		}

	}

	return res
}

type Job struct {
	id        string
	remaining int
}

func fs2(input io.Reader, stepTime, nWorkers int) int {
	scanner := bufio.NewScanner(input)

	befores := make(map[string][]string)
	for scanner.Scan() {
		line := scanner.Text()
		del := lib.NewDelimiter(line, " ")
		from := del.GetString(1)
		to := del.GetString(7)
		befores[from] = append(befores[from], to)
	}
	total := len(befores)

	inDegree := make(map[string]int)
	for k, values := range befores {
		inDegree[k] = 0
		for _, v := range values {
			inDegree[v] = 0
		}
	}

	for _, values := range befores {
		for _, v := range values {
			inDegree[v]++
		}
	}

	var availableJobs []string
	for k, v := range inDegree {
		if v == 0 {
			availableJobs = append(availableJobs, k)
		}
	}

	sum := 0
	workers := make([]Job, nWorkers)
	for total >= 0 {
		freeWorkers := getNumberFreeWorkers(workers)
		if freeWorkers != nWorkers {
			for {
				sum++
				toStop := false
				for i := 0; i < len(workers); i++ {
					if workers[i].remaining == 0 {
						continue
					}
					workers[i].remaining--
					if workers[i].remaining == 0 {
						toStop = true
						total--
						for _, values := range befores[workers[i].id] {
							inDegree[values]--
							if inDegree[values] == 0 {
								availableJobs = append(availableJobs, values)
							}
						}
					}
				}
				if toStop {
					break
				}
			}
		}

		for len(availableJobs) != 0 {
			sort.Strings(availableJobs)

			available := len(availableJobs)
			freeWorkers := getNumberFreeWorkers(workers)

			if available == 1 && freeWorkers == nWorkers {
				v := availableJobs[0]
				availableJobs = availableJobs[1:]
				total--
				for _, values := range befores[v] {
					inDegree[values]--
					if inDegree[values] == 0 {
						availableJobs = append(availableJobs, values)
					}
				}
				sum += toSecond(v, stepTime)
				continue
			}

			jobs := lib.Min(available, freeWorkers)
			for i := 0; i < jobs; i++ {
				v := availableJobs[0]
				availableJobs = availableJobs[1:]
				id := idNextFreeWorker(workers)
				workers[id] = Job{id: v, remaining: toSecond(v, stepTime)}
			}

			for {
				sum++
				toStop := false
				for i := 0; i < len(workers); i++ {
					if workers[i].remaining == 0 {
						continue
					}
					workers[i].remaining--
					if workers[i].remaining == 0 {
						toStop = true
						total--
						for _, values := range befores[workers[i].id] {
							inDegree[values]--
							if inDegree[values] == 0 {
								availableJobs = append(availableJobs, values)
							}
						}
					}
				}
				if toStop {
					break
				}
			}

		}
	}

	for _, worker := range workers {
		sum += worker.remaining
	}

	return sum
}

func idNextFreeWorker(workers []Job) int {
	for i, job := range workers {
		if isFree(job) {
			return i
		}
	}
	panic(workers)
}

func getNumberFreeWorkers(workers []Job) int {
	sum := 0
	for _, job := range workers {
		if isFree(job) {
			sum++
		}
	}
	return sum
}

func isFree(worker Job) bool {
	return worker.remaining == 0
}

func toSecond(s string, step int) int {
	return int(s[0]-'A') + 1 + step
}
