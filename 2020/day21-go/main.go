package main

import (
	"bufio"
	"io"
	"sort"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var r []Recipe
	for scanner.Scan() {
		line := scanner.Text()
		r = append(r, toRecipe(line))
	}

	recipes := make(map[string][]map[string]bool)
	for _, recipe := range r {
		for _, allergen := range recipe.allergens {
			m := make(map[string]bool)
			for _, v := range recipe.ingredients {
				m[v] = true
			}
			recipes[allergen] = append(recipes[allergen], m)
		}
	}

	remainingAllergens := make(map[string]bool)
	for k := range recipes {
		remainingAllergens[k] = true
	}

	foundAllergenForIngredient := make(map[string]string)

	for len(remainingAllergens) != 0 {
		for allergen := range remainingAllergens {
			intersections := findIntersections(recipes[allergen])
			if len(intersections) == 1 {
				delete(remainingAllergens, allergen)
				foundAllergenForIngredient[intersections[0]] = allergen
				for _, rcps := range recipes {
					for _, rcp := range rcps {
						delete(rcp, intersections[0])
					}
				}
			}
		}
	}

	sum := 0
	for _, recipe := range r {
		for _, ingredient := range recipe.ingredients {
			if _, exists := foundAllergenForIngredient[ingredient]; !exists {
				sum++
			}
		}
	}

	return sum
}

func findIntersections(recipes []map[string]bool) []string {
	contains := make(map[string]bool)
	for i := 0; i < len(recipes[0]); i++ {
		for ingredient := range recipes[0] {
			contains[ingredient] = true
		}
	}

	for i := 1; i < len(recipes); i++ {
		m := make(map[string]bool)
		for k := range recipes[i] {
			m[k] = true
		}

		for k := range contains {
			if !m[k] {
				delete(contains, k)
			}
		}
	}

	var res []string
	for k := range contains {
		res = append(res, k)
	}
	return res
}

type Recipe struct {
	ingredients []string
	allergens   []string
}

func toRecipe(s string) Recipe {
	sep := strings.Index(s, "(")
	ingredients := aoc.NewDelimiter(s[:sep-1], " ").GetStrings()
	contains := aoc.NewDelimiter(s[sep+10:len(s)-1], ", ").GetStrings()
	return Recipe{
		ingredients: ingredients,
		allergens:   contains,
	}
}

func fs2(input io.Reader) string {
	scanner := bufio.NewScanner(input)
	var r []Recipe
	for scanner.Scan() {
		line := scanner.Text()
		r = append(r, toRecipe(line))
	}

	recipes := make(map[string][]map[string]bool)
	for _, recipe := range r {
		for _, allergen := range recipe.allergens {
			m := make(map[string]bool)
			for _, v := range recipe.ingredients {
				m[v] = true
			}
			recipes[allergen] = append(recipes[allergen], m)
		}
	}

	remainingAllergens := make(map[string]bool)
	for k := range recipes {
		remainingAllergens[k] = true
	}

	foundAllergenForIngredient := make(map[string]string)

	for len(remainingAllergens) != 0 {
		for allergen := range remainingAllergens {
			intersections := findIntersections(recipes[allergen])
			if len(intersections) == 1 {
				delete(remainingAllergens, allergen)
				foundAllergenForIngredient[intersections[0]] = allergen
				for _, rcps := range recipes {
					for _, rcp := range rcps {
						delete(rcp, intersections[0])
					}
				}
			}
		}
	}

	var founds []Found
	for k, v := range foundAllergenForIngredient {
		founds = append(founds, Found{
			allergen:   v,
			ingredient: k,
		})
	}

	sort.Slice(founds, func(i, j int) bool {
		a := founds[i]
		b := founds[j]
		return strings.Compare(a.allergen, b.allergen) < 0
	})

	var res []string
	for _, found := range founds {
		res = append(res, found.ingredient)
	}

	return strings.Join(res, ",")
}

type Found struct {
	allergen   string
	ingredient string
}
