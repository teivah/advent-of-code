package main

import (
	"bufio"
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var actions []Action
	for scanner.Scan() {
		line := scanner.Text()
		actions = append(actions, toAction(line))
	}

	ship := newShip(actions)
	ship.seal()
	return ship.ship.ManhattanZero()
}

type Ship struct {
	direction aoc.Direction
	ship      aoc.Position
	waypoint  aoc.Position
	actions   []Action
}

func newShip(actions []Action) *Ship {
	return &Ship{
		direction: aoc.Right,
		actions:   actions,
		waypoint:  aoc.Position{-1, 10},
	}
}

func (s *Ship) seal() {
	for _, action := range s.actions {
		switch action.actionType {
		case north:
			s.ship = s.ship.Move(aoc.Up, action.unit)
		case south:
			s.ship = s.ship.Move(aoc.Down, action.unit)
		case east:
			s.ship = s.ship.Move(aoc.Right, action.unit)
		case west:
			s.ship = s.ship.Move(aoc.Left, action.unit)
		case turnLeft:
			for i := 0; i < action.unit/90; i++ {
				s.direction = s.direction.Turn(aoc.Left)
			}
		case turnRight:
			for i := 0; i < action.unit/90; i++ {
				s.direction = s.direction.Turn(aoc.Right)
			}
		case forward:
			s.ship = s.ship.Move(s.direction, action.unit)
		}
	}
}

type Action struct {
	actionType ActionType
	unit       int
}

func (a Action) String() string {
	return fmt.Sprintf("%c%d", a.actionType, a.unit)
}

func toAction(s string) Action {
	return Action{
		actionType: ActionType(s[0]),
		unit:       aoc.StringToInt(s[1:]),
	}
}

type ActionType rune

const (
	north     ActionType = 'N'
	south     ActionType = 'S'
	east      ActionType = 'E'
	west      ActionType = 'W'
	turnLeft  ActionType = 'L'
	turnRight ActionType = 'R'
	forward   ActionType = 'F'
)

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var actions []Action
	for scanner.Scan() {
		line := scanner.Text()
		actions = append(actions, toAction(line))
	}

	ship := newShip(actions)
	ship.seal2()
	return ship.ship.ManhattanZero()
}

func (s *Ship) seal2() {
	for _, action := range s.actions {
		switch action.actionType {
		case north:
			s.waypoint = s.waypoint.Move(aoc.Up, action.unit)
		case south:
			s.waypoint = s.waypoint.Move(aoc.Down, action.unit)
		case east:
			s.waypoint = s.waypoint.Move(aoc.Right, action.unit)
		case west:
			s.waypoint = s.waypoint.Move(aoc.Left, action.unit)
		case turnLeft:
			for i := 0; i < action.unit/90; i++ {
				s.waypoint.Col, s.waypoint.Row = s.waypoint.Row, -s.waypoint.Col
			}
		case turnRight:
			for i := 0; i < action.unit/90; i++ {
				s.waypoint.Col, s.waypoint.Row = -s.waypoint.Row, s.waypoint.Col
			}
		case forward:
			if s.waypoint.Row != 0 {
				if s.waypoint.Row < 0 {
					s.ship = s.ship.Move(aoc.Up, action.unit*aoc.Abs(s.waypoint.Row))
				} else {
					s.ship = s.ship.Move(aoc.Down, action.unit*aoc.Abs(s.waypoint.Row))
				}
			}
			if s.waypoint.Col != 0 {
				if s.waypoint.Col < 0 {
					s.ship = s.ship.Move(aoc.Left, action.unit*aoc.Abs(s.waypoint.Col))
				} else {
					s.ship = s.ship.Move(aoc.Right, action.unit*aoc.Abs(s.waypoint.Col))
				}
			}
		}
	}
}
