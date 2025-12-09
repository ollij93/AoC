package day1

import (
	"strconv"
	"strings"
)

type Solution struct{}

func (*Solution) Solve(inp string) (uint, uint) {
	puzzle := parse(inp)
	return puzzle.solve()
}

type puzzle struct {
	nums []int
}

func (puzz *puzzle) solve() (retA, retB uint) {
	freq := 0
	freqs := make(map[int]struct{})
	for retB == 0 {
		for _, num := range puzz.nums {
			freq += num
			_, exists := freqs[freq]
			if exists {
				if retB == 0 {
					retB = uint(freq)
				}
			} else {
				freqs[freq] = struct{}{}
			}
		}

		if retA == 0 {
			retA = uint(freq)
		}
	}
	return
}

func parse(inp string) puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	nums := make([]int, len(lines))
	for i, line := range lines {
		nums[i], _ = strconv.Atoi(line)
	}
	return puzzle{nums}
}
