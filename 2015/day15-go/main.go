package main

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader, teaspoons int) (int, error) {
	scanner := bufio.NewScanner(input)
	var ingredients []Ingredient
	for scanner.Scan() {
		s := scanner.Text()
		ingredient, err := toIngredient(s)
		if err != nil {
			return 0, err
		}
		ingredients = append(ingredients, ingredient)
	}

	w := worker{ingredients: ingredients, cache: make(map[int]int)}

	return w.getBest(0, make([]int, teaspoons)), nil
}

type worker struct {
	ingredients []Ingredient
	cache       map[int]int
}

func (w *worker) getBest(index int, teaspoons []int) int {
	if index == len(teaspoons) {
		return w.sum(teaspoons)
	}

	k := w.formatKey(teaspoons)
	if v, exists := w.cache[k]; exists {
		return v
	}

	best := -1
	for i := range w.ingredients {
		teaspoons[index] = i
		best = max(best, w.getBest(index+1, teaspoons))
	}
	w.cache[k] = best
	return best
}

func (w *worker) formatKey(teaspoons []int) int {
	total := make([]int, len(w.ingredients))
	for _, teaspoon := range teaspoons {
		total[teaspoon]++
	}

	k := 0
	shift := 0
	for _, t := range total {
		k += t << shift
		shift += 8
	}
	return k
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func (w *worker) sum(teaspoons []int) int {
	capacity := 0
	durability := 0
	flavor := 0
	texture := 0

	for _, teaspoon := range teaspoons {
		capacity += w.ingredients[teaspoon].capacity
		durability += w.ingredients[teaspoon].durability
		flavor += w.ingredients[teaspoon].flavor
		texture += w.ingredients[teaspoon].texture
	}

	if capacity < 0 {
		return 0
	}
	if durability < 0 {
		return 0
	}
	if flavor < 0 {
		return 0
	}
	if texture < 0 {
		return 0
	}
	return capacity * durability * flavor * texture
}

func (w *worker) sum2(teaspoons []int) int {
	capacity := 0
	durability := 0
	flavor := 0
	texture := 0
	calories := 0

	for _, teaspoon := range teaspoons {
		capacity += w.ingredients[teaspoon].capacity
		durability += w.ingredients[teaspoon].durability
		flavor += w.ingredients[teaspoon].flavor
		texture += w.ingredients[teaspoon].texture
		calories += w.ingredients[teaspoon].calories
	}

	if capacity < 0 {
		return 0
	}
	if durability < 0 {
		return 0
	}
	if flavor < 0 {
		return 0
	}
	if texture < 0 {
		return 0
	}

	if calories != 500 {
		return 0
	}

	return capacity * durability * flavor * texture
}

type Ingredient struct {
	capacity   int
	durability int
	flavor     int
	texture    int
	calories   int
}

func toIngredient(s string) (Ingredient, error) {
	idx := indexAll(s, " ")
	capacity, err := strconv.Atoi(s[idx[1]+1 : idx[2]-1])
	if err != nil {
		return Ingredient{}, err
	}
	durability, err := strconv.Atoi(s[idx[3]+1 : idx[4]-1])
	if err != nil {
		return Ingredient{}, err
	}
	flavor, err := strconv.Atoi(s[idx[5]+1 : idx[6]-1])
	if err != nil {
		return Ingredient{}, err
	}
	texture, err := strconv.Atoi(s[idx[7]+1 : idx[8]-1])
	if err != nil {
		return Ingredient{}, err
	}
	calories, err := strconv.Atoi(s[idx[9]+1:])
	if err != nil {
		return Ingredient{}, err
	}
	return Ingredient{
		capacity:   capacity,
		durability: durability,
		flavor:     flavor,
		texture:    texture,
		calories:   calories,
	}, nil
}

func fs2(input io.Reader, teaspoons int) (int, error) {
	scanner := bufio.NewScanner(input)
	var ingredients []Ingredient
	for scanner.Scan() {
		s := scanner.Text()
		ingredient, err := toIngredient(s)
		if err != nil {
			return 0, err
		}
		ingredients = append(ingredients, ingredient)
	}

	w := worker{ingredients: ingredients, cache: make(map[int]int)}

	return w.getBest2(0, make([]int, teaspoons)), nil
}

func (w *worker) getBest2(index int, teaspoons []int) int {
	if index == len(teaspoons) {
		return w.sum2(teaspoons)
	}

	k := w.formatKey(teaspoons)
	if v, exists := w.cache[k]; exists {
		return v
	}

	best := -1
	for i := range w.ingredients {
		teaspoons[index] = i
		best = max(best, w.getBest2(index+1, teaspoons))
	}
	w.cache[k] = best
	return best
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
