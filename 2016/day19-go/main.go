package main

func fs1(elves int) int {
	root := &Elf{
		id:       1,
		presents: 1,
	}
	previous := root

	for i := 2; i <= elves; i++ {
		elf := &Elf{
			id:       i,
			presents: 1,
		}
		previous.left = elf
		previous = elf

		if i == elves {
			elf.left = root
		}
	}

	cur := root
	for {
		if cur.isOver(elves) {
			return cur.id
		}

		cur.stealLeft()
		cur = cur.left
	}
}

type Elf struct {
	id       int
	presents int
	left     *Elf
}

func (e *Elf) isOver(elves int) bool {
	return e.presents == elves
}

func (e *Elf) stealLeft() {
	if e.presents == 0 {
		return
	}

	e.presents += e.left.presents
	e.left = e.left.left
}

func (e *Elf) stealMiddle(remaining int) {
	moves := 0
	if remaining%2 == 1 {
		moves = remaining / 2
	} else {
		moves = remaining/2 - 1
	}

	var previous *Elf
	target := e
	for i := 0; i < moves; i++ {
		previous = target
		target = target.left
	}

	e.presents += target.presents
	previous.left = target.left
}

func fs2(elves int) int {
	circle := make([]int, elves)

	for i := 0; i < elves; i++ {
		circle[i] = i + 1
	}

	for i := 0; ; {
		if elves == 2 {
			return circle[i]
		}

		next := 0
		next = elves / 2

		next = (i + next) % elves
		if next == 0 {
			circle = circle[1:]
		} else if next == elves-1 {
			circle = circle[:elves-1]
		} else {
			circle = append(circle[:next], circle[next+1:]...)
		}

		elves--
		if next < i {
			i = i % elves
		} else {
			i = (i + 1) % elves
		}
	}

}
