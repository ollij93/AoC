package main

import (
	"log/slog"
	"strconv"
	"strings"
)

type Day1Solution struct{}

func (*Day1Solution) Solve(inp string) (int, int) {
	puzzle := parse1(inp)
	return puzzle.solve()
}

type day1Puzzle struct {
	rots []int
}

func (puzz *day1Puzzle) solve() (int, int) {
	retA := 0
	retB := 0
	curr := 50
	for i := range puzz.rots {
		rot := puzz.rots[i]
		new := curr + rot
		slog.Debug("", "From", curr, "to", new, "via", rot)
		if new < 0 {
			// Went left enough to go past zero
			// How many times? = 1 plus number of hundreds
			n := 1 + ((new * -1) / 100)
			// Account for starting on zero as we didn't "pass" 0
			if curr == 0 {
				n -= 1
			}
			retB += n
			slog.Debug("  neg", "n", n)
		} else if new >= 100 {
			// Went right enough to go past zero
			// How many times? = number of hundreds
			n := (new / 100)
			retB += n
			slog.Debug("  pos", "n", n)
		} else if new == 0 {
			slog.Debug("  hit", "n", 1)
			retB += 1
		}

		// Handle wrapping of value
		for new < 0 {
			new += 100
		}
		curr = new % 100

		// Counting for part1
		if curr == 0 {
			retA += 1
		}
	}
	return retA, retB
}

func parse1(inp string) day1Puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")

	rots := make([]int, len(lines))
	for i := range lines {
		line := lines[i]
		val, _ := strconv.Atoi(line[1:])
		if line[0] == 'L' {
			val *= -1
		}
		rots[i] = val
	}
	return day1Puzzle{rots: rots}
}
