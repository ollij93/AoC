package main

import "strings"

type Day7Solution struct{}

func (*Day7Solution) Solve(inp string) (uint, uint) {
	puzzle := parse7(inp)
	return puzzle.solve()
}

type day7Puzzle struct {
	startIdx uint
	layers   [](map[uint]struct{}) // Set of positions with splitters
}

func (puzz *day7Puzzle) solve() (retA, retB uint) {
	beam := make(map[uint]uint) // Map of positions to count of timelines
	beam[puzz.startIdx] = 1

	for _, layer := range puzz.layers {
		newbeam := make(map[uint]uint)
		for point := range beam {
			_, in := layer[point]
			if in {
				newbeam[point-1] += beam[point]
				newbeam[point+1] += beam[point]
				retA += 1
			} else {
				newbeam[point] += beam[point]
			}
		}
		beam = newbeam
	}
	for idx := range beam {
		retB += beam[idx]
	}
	return
}

func parse7(inp string) day7Puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	firstLine := lines[0]
	startIdx := uint(strings.Index(firstLine, "S"))
	sets := make([]map[uint]struct{}, len(lines)-1)
	for i, line := range lines[1:] {
		sets[i] = make(map[uint]struct{})
		for idx, byt := range line {
			if byt == '^' {
				// Push index into the set
				sets[i][uint(idx)] = struct{}{}
			}
		}
	}

	return day7Puzzle{startIdx, sets}
}
