package main

import (
	"log/slog"
	"strings"
)

type Day4Solution struct{}

func (*Day4Solution) Solve(inp string) (int, int) {
	puzzle := parse4(inp)
	return puzzle.solve()
}

type day4Puzzle struct {
	width  uint
	height uint
	array  []bool
}

func (puzz *day4Puzzle) get(x uint, y uint) bool {
	// Get whether a roll is at position X,Y
	index := x + puzz.width*y
	return puzz.array[index]
}

func (puzz *day4Puzzle) occupiedYNeighbours(x uint, y uint) (ret uint) {
	// Check (X, Y-1), (X, Y) and (X, Y+1) returning the number that are empty
	// If off the edge of the puzzle, the space is treated as empty
	if y > 0 && puzz.get(x, y-1) {
		ret += 1
	}
	if puzz.get(x, y) {
		ret += 1
	}
	if y < puzz.height-1 && puzz.get(x, y+1) {
		ret += 1
	}
	return
}

func (puzz *day4Puzzle) isAccessible(x uint, y uint) bool {
	if !puzz.get(x, y) {
		return false
	}

	occupiedNeighbours := uint(0)
	if x > 0 {
		occupiedNeighbours += puzz.occupiedYNeighbours(x-1, y)
	}
	occupiedNeighbours += puzz.occupiedYNeighbours(x, y) - 1
	if x < puzz.width-1 {
		occupiedNeighbours += puzz.occupiedYNeighbours(x+1, y)
	}
	return occupiedNeighbours < 4
}

func (puzz *day4Puzzle) solve() (int, int) {
	thisArray := puzz.array
	thisPuzz := day4Puzzle{
		width:  puzz.width,
		height: puzz.height,
		array:  thisArray,
	}
	nextArray := make([]bool, len(puzz.array))
	retA := 0
	retB := 0
	i := 0
	for {
		anyAccessible := false
		for y := range puzz.height {
			for x := range puzz.width {
				index := x + puzz.width*y
				nextArray[index] = thisArray[index]
				if thisPuzz.isAccessible(x, y) {
					anyAccessible = true
					retB += 1
					nextArray[index] = false
					if i == 0 {
						retA += 1
					}
				}
			}
		}
		if !anyAccessible {
			break
		}

		thisArray = nextArray
		thisPuzz = day4Puzzle{
			width:  puzz.width,
			height: puzz.height,
			array:  thisArray,
		}
		nextArray = make([]bool, len(puzz.array))
		i++
	}
	return int(retA), retB
}

func parse4(inp string) day4Puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	width := uint(len(lines[0]))
	height := uint(len(lines))
	rawarray := strings.ReplaceAll(cleaninp, "\n", "")
	array := make([]bool, len(rawarray))
	for i, val := range rawarray {
		array[i] = (val == '@')
	}
	slog.Debug("Puzzle", "width", width, "height", height)
	return day4Puzzle{width: width, height: height, array: array}
}
