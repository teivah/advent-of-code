package main

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader, res string) (int, error) {
	scanner := bufio.NewScanner(input)
	var insts []Inst
	gates := make(map[string]uint16)
	for scanner.Scan() {
		s := scanner.Text()
		if len(indexAll(s, " ")) == 2 {
			id := indexAll(s, " ")
			first, err := toOperand(s[:id[0]])
			if err != nil {
				return 0, err
			}
			insts = append(
				insts, Inst{
					instType: Signal,
					first:    first,
					gate:     s[id[1]+1:],
				},
			)
		} else {
			if s[0] == 'N' {
				id := indexAll(s, " ")
				insts = append(
					insts, Inst{
						instType: Not,
						first: Operand{
							gate: s[id[0]+1 : id[1]],
						},
						gate: s[id[2]+1:],
					},
				)
			} else {
				id := strings.Index(s, " ")
				switch s[id+1] {
				case 'A':
					id := indexAll(s, " ")
					first, err := toOperand(s[:id[0]])
					if err != nil {
						return 0, err
					}
					second, err := toOperand(s[id[1]+1 : id[2]])
					if err != nil {
						return 0, err
					}
					insts = append(
						insts, Inst{
							instType: And,
							first:    first,
							second:   second,
							gate:     s[id[3]+1:],
						},
					)
				case 'O':
					id := indexAll(s, " ")
					first, err := toOperand(s[:id[0]])
					if err != nil {
						return 0, err
					}
					second, err := toOperand(s[id[1]+1 : id[2]])
					if err != nil {
						return 0, err
					}
					insts = append(
						insts, Inst{
							instType: Or,
							first:    first,
							second:   second,
							gate:     s[id[3]+1:],
						},
					)
				case 'L':
					id := indexAll(s, " ")
					first, err := toOperand(s[:id[0]])
					if err != nil {
						return 0, err
					}
					second, err := toOperand(s[id[1]+1 : id[2]])
					if err != nil {
						return 0, err
					}
					insts = append(
						insts, Inst{
							instType: LShift,
							first:    first,
							second:   second,
							gate:     s[id[3]+1:],
						},
					)
				case 'R':
					id := indexAll(s, " ")
					first, err := toOperand(s[:id[0]])
					if err != nil {
						return 0, err
					}
					second, err := toOperand(s[id[1]+1 : id[2]])
					if err != nil {
						return 0, err
					}
					insts = append(
						insts, Inst{
							instType: RShift,
							first:    first,
							second:   second,
							gate:     s[id[3]+1:],
						},
					)
				}
			}
		}
		gates[insts[len(insts)-1].gate] = 0
	}

	graph := make(map[string][]string)
	inDegree := make(map[string]int)
	for gate := range gates {
		inDegree[gate] = 0
	}
	mInsts := make(map[string]Inst)
	for _, inst := range insts {
		gate := inst.gate
		mInsts[gate] = inst
		if !inst.first.isValue {
			graph[inst.first.gate] = append(graph[inst.first.gate], gate)
		}
		if inst.instType == And || inst.instType == Or || inst.instType == LShift || inst.instType == RShift {
			if !inst.second.isValue {
				graph[inst.second.gate] = append(graph[inst.second.gate], gate)
			}
		}
	}
	for parent, children := range graph {
		_ = parent
		for _, child := range children {
			inDegree[child]++
		}
	}
	var q []string
	for gate := range gates {
		if inDegree[gate] == 0 {
			q = append(q, gate)
		}
	}

	var order []string
	for len(q) != 0 {
		gate := q[0]
		q = q[1:]
		order = append(order, gate)
		children := graph[gate]
		for _, child := range children {
			inDegree[child]--
			if inDegree[child] == 0 {
				q = append(q, child)
			}
		}
	}

	for _, gate := range order {
		calc(gates, mInsts[gate])
	}

	return int(gates[res]), nil
}

func calc(gates map[string]uint16, inst Inst) string {
	gate := inst.gate
	switch inst.instType {
	case Signal:
		gates[gate] = inst.first.getValue(gates)
	case And:
		gates[gate] = inst.first.getValue(gates) & inst.second.getValue(gates)
	case Or:
		gates[gate] = inst.first.getValue(gates) | inst.second.getValue(gates)
	case LShift:
		gates[gate] = inst.first.getValue(gates) << inst.second.getValue(gates)
	case RShift:
		gates[gate] = inst.first.getValue(gates) >> inst.second.getValue(gates)
	case Not:
		gates[gate] = ^inst.first.getValue(gates)
	}
	return gate
}

func indexAll(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

type Inst struct {
	instType InstType
	first    Operand
	second   Operand
	gate     string
}

type Operand struct {
	isValue bool
	value   uint16
	gate    string
}

func (o Operand) getValue(gates map[string]uint16) uint16 {
	if o.isValue {
		return o.value
	}
	return gates[o.gate]
}

func toOperand(s string) (Operand, error) {
	if s[0] >= '0' && s[0] <= '9' {
		i, err := strconv.Atoi(s)
		if err != nil {
			return Operand{}, err
		}
		return Operand{
			isValue: true,
			value:   uint16(i),
		}, nil
	}
	return Operand{
		gate: s,
	}, nil
}

type InstType int

const (
	Signal InstType = iota
	And
	Or
	LShift
	RShift
	Not
)

func fs2(input io.Reader, res, override string) (int, error) {
	scanner := bufio.NewScanner(input)
	var insts []Inst
	gates := make(map[string]uint16)
	for scanner.Scan() {
		s := scanner.Text()
		if len(indexAll(s, " ")) == 2 {
			id := indexAll(s, " ")
			first, err := toOperand(s[:id[0]])
			if err != nil {
				return 0, err
			}
			insts = append(
				insts, Inst{
					instType: Signal,
					first:    first,
					gate:     s[id[1]+1:],
				},
			)
		} else {
			if s[0] == 'N' {
				id := indexAll(s, " ")
				insts = append(
					insts, Inst{
						instType: Not,
						first: Operand{
							gate: s[id[0]+1 : id[1]],
						},
						gate: s[id[2]+1:],
					},
				)
			} else {
				id := strings.Index(s, " ")
				switch s[id+1] {
				case 'A':
					id := indexAll(s, " ")
					first, err := toOperand(s[:id[0]])
					if err != nil {
						return 0, err
					}
					second, err := toOperand(s[id[1]+1 : id[2]])
					if err != nil {
						return 0, err
					}
					insts = append(
						insts, Inst{
							instType: And,
							first:    first,
							second:   second,
							gate:     s[id[3]+1:],
						},
					)
				case 'O':
					id := indexAll(s, " ")
					first, err := toOperand(s[:id[0]])
					if err != nil {
						return 0, err
					}
					second, err := toOperand(s[id[1]+1 : id[2]])
					if err != nil {
						return 0, err
					}
					insts = append(
						insts, Inst{
							instType: Or,
							first:    first,
							second:   second,
							gate:     s[id[3]+1:],
						},
					)
				case 'L':
					id := indexAll(s, " ")
					first, err := toOperand(s[:id[0]])
					if err != nil {
						return 0, err
					}
					second, err := toOperand(s[id[1]+1 : id[2]])
					if err != nil {
						return 0, err
					}
					insts = append(
						insts, Inst{
							instType: LShift,
							first:    first,
							second:   second,
							gate:     s[id[3]+1:],
						},
					)
				case 'R':
					id := indexAll(s, " ")
					first, err := toOperand(s[:id[0]])
					if err != nil {
						return 0, err
					}
					second, err := toOperand(s[id[1]+1 : id[2]])
					if err != nil {
						return 0, err
					}
					insts = append(
						insts, Inst{
							instType: RShift,
							first:    first,
							second:   second,
							gate:     s[id[3]+1:],
						},
					)
				}
			}
		}
		gates[insts[len(insts)-1].gate] = 0
	}

	graph := make(map[string][]string)
	inDegree := make(map[string]int)
	for gate := range gates {
		inDegree[gate] = 0
	}
	mInsts := make(map[string]Inst)
	for _, inst := range insts {
		gate := inst.gate
		mInsts[gate] = inst
		if !inst.first.isValue {
			graph[inst.first.gate] = append(graph[inst.first.gate], gate)
		}
		if inst.instType == And || inst.instType == Or || inst.instType == LShift || inst.instType == RShift {
			if !inst.second.isValue {
				graph[inst.second.gate] = append(graph[inst.second.gate], gate)
			}
		}
	}
	for parent, children := range graph {
		_ = parent
		for _, child := range children {
			inDegree[child]++
		}
	}
	var q []string
	for gate := range gates {
		if inDegree[gate] == 0 {
			q = append(q, gate)
		}
	}

	var order []string
	for len(q) != 0 {
		gate := q[0]
		q = q[1:]
		order = append(order, gate)
		children := graph[gate]
		for _, child := range children {
			inDegree[child]--
			if inDegree[child] == 0 {
				q = append(q, child)
			}
		}
	}

	for _, gate := range order {
		calc(gates, mInsts[gate])
	}

	v := gates[res]
	for k := range gates {
		gates[k] = 0
	}
	gates[override] = v

	for _, gate := range order {
		if gate == override {
			continue
		}
		calc(gates, mInsts[gate])
	}

	return int(gates[res]), nil
}
