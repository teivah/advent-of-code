package aoc

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

// ReaderToString converts an io.Reader into a string.
func ReaderToString(input io.Reader) string {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	return scanner.Text()
}

// ReaderToStrings converts an io.Reader into a slice of strings.
func ReaderToStrings(input io.Reader) []string {
	scanner := bufio.NewScanner(input)
	var res []string
	for scanner.Scan() {
		res = append(res, scanner.Text())
	}
	return res
}

// ReaderToInts converts optimistically an io.Reader into a slice of strings.
func ReaderToInts(input io.Reader) []int {
	scanner := bufio.NewScanner(input)
	var res []int
	for scanner.Scan() {
		res = append(res, StringToInt(scanner.Text()))
	}
	return res
}

// Delimiter implementation.
type Delimiter struct {
	ind []int
	s   string
	del       string
	trimSpace bool
}

type delimiterOptions struct {
	trimSpace bool
}

// DelimiterOption holds the Delimiter options.
type DelimiterOption func(options *delimiterOptions)

// WithTrimSpace applies strings.trimSpace on each string.
func WithTrimSpace() DelimiterOption {
	return func(options *delimiterOptions) {
		options.trimSpace = true
	}
}

// NewDelimiter creates a new input delimiter logic.
func NewDelimiter(s, del string, opts ...DelimiterOption) Delimiter {
	var options delimiterOptions
	for _, opt := range opts {
		opt(&options)
	}

	return Delimiter{
		ind:       IndexAll(s, del),
		s:         s,
		del:       del,
		trimSpace: options.trimSpace,
	}
}

// GetStrings returns all the strings found.
func (d Delimiter) GetStrings() []string {
	if len(d.ind) == 0 {
		if d.s == "" {
			return nil
		}

		if d.trimSpace {
			return []string{strings.TrimSpace(d.s)}
		}
		return []string{d.s}
	}

	var res []string
	for i := 0; i <= len(d.ind); i++ {
		s := d.GetString(i)
		if s == "" {
			continue
		}
		res = append(res, s)
	}

	return res
}

// GetInts returns all the ints found with an optimistic conversion.
func (d Delimiter) GetInts() []int {
	return StringsToInts(d.GetStrings())
}

// GetString returns the string at a given index.
func (d Delimiter) GetString(i int) string {
	s := ""
	if i == 0 {
		s = d.s[:d.ind[0]]
	} else if i == len(d.ind) {
		s = d.s[d.ind[len(d.ind)-1]+len(d.del):]
	} else {
		s = d.s[d.ind[i-1]+len(d.del) : d.ind[i]]
	}

	if d.trimSpace {
		return strings.TrimSpace(s)
	}
	return s
}

// GetInt returns the int at a given index with an optimistic conversion.
func (d Delimiter) GetInt(i int) int {
	return StringToInt(d.GetString(i))
}

// TryGetInt returns the int at a given index.
func (d Delimiter) TryGetInt(i int) (int, bool) {
	return TryStringToInt(d.GetString(i))
}

// IsInt checks whether the value at a given index is an int.
func (d Delimiter) IsInt(i int) bool {
	_, err := strconv.Atoi(d.GetString(i))
	return err == nil
}

// StringGroups returns groups of lines inputs that are not separated by empty
// lines.
//
// For example:
//
// 0, 1, 2
//
// foo
// bar
//
// Returns {"0, 1, 2"} and {"foo", "bar"}
func StringGroups(lines []string) [][]string {
	i := 0
	var res [][]string
	var row []string
	for {
		row = append(row, lines[i])
		i++
		if i >= len(lines) {
			res = append(res, row)
			break
		}
		for ; i < len(lines); i++ {
			if lines[i] == "" {
				break
			} else {
				row = append(row, lines[i])
			}
		}
		res = append(res, row)
		row = nil
		i++
		if i >= len(lines) {
			break
		}
	}
	return res
}
