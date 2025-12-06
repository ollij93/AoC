package main

import (
	"log/slog"
	"strconv"
	"strings"
)

type Day6Solution struct{}

func (*Day6Solution) Solve(inp string) (uint, uint) {
	puzzle := parse6(inp)
	return puzzle.solve()
}

type day6sum struct {
	parts []uint
	mult  bool
}

type day6Puzzle struct {
	sumsA []day6sum
	sumsB []day6sum
}

func (puzz *day6Puzzle) solve() (retA, retB uint) {
	for _, sum := range puzz.sumsA {
		curr := uint(0)
		if sum.mult {
			curr = 1
		}
		for _, num := range sum.parts {
			if sum.mult {
				curr *= num
			} else {
				curr += num
			}
		}
		slog.Debug("SUMA", "sum", sum, "res", curr)
		retA += curr
	}
	for _, sum := range puzz.sumsB {
		curr := uint(0)
		if sum.mult {
			curr = 1
		}
		for _, num := range sum.parts {
			if sum.mult {
				curr *= num
			} else {
				curr += num
			}
		}
		slog.Debug("SUMB", "sum", sum, "res", curr)
		retB += curr
	}
	return
}

func parse6A(inp string) []day6sum {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	allParts := make([][]string, len(lines))
	for linei := range lines {
		allParts[linei] = strings.Fields(lines[linei])
	}
	signs := allParts[len(allParts)-1]
	parts := allParts[:len(allParts)-1]
	sums := make([]day6sum, len(parts[0]))
	for sumi := range sums {
		sumParts := make([]uint, len(parts))
		for pi := range parts {
			val, _ := strconv.Atoi(parts[pi][sumi])
			sumParts[pi] = uint(val)
		}
		mult := signs[sumi] == "*"
		sums[sumi] = day6sum{sumParts, mult}
	}
	return sums
}

func parse6B(inp string) []day6sum {
	// Note: first character isn't white-space (by chance)
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	lastLine := lines[len(lines)-1]
	signs := strings.Fields(lastLine)
	lines = lines[:len(lines)-1]

	transposedLines := make([]string, len(lines[0]))
	for i := range transposedLines {
		parts := make([]byte, len(lines))
		for li := range lines {
			parts[li] = lines[li][i]
		}
		transposedLines[i] = strings.TrimSpace(string(parts))
	}
	sumSegments := strings.Split(strings.Join(transposedLines, "\n"), "\n\n")
	sums := make([]day6sum, len(sumSegments))
	for sumi, seg := range sumSegments {
		parts := strings.Split(seg, "\n")
		values := make([]uint, len(parts))
		for i, part := range parts {
			val, _ := strconv.Atoi(part)
			values[i] = uint(val)
		}
		sums[sumi] = day6sum{values, signs[sumi] == "*"}
	}
	return sums
}

func parse6(inp string) day6Puzzle {
	sumsA := parse6A(inp)
	sumsB := parse6B(inp)
	return day6Puzzle{sumsA, sumsB}
}
