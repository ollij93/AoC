package day1

import (
	"fmt"
	"strconv"
	"strings"
)

type Solution struct{}

func (*Solution) Solve(inp string) (string, string) {
	puzzle := parse(inp)
	return puzzle.solve()
}

type puzzle struct {
	nums []int
}

func (puzz *puzzle) solve() (retA, retB string) {
	freq := 0
	freqs := make(map[int]struct{})
	for retB == "" {
		for _, num := range puzz.nums {
			freq += num
			_, exists := freqs[freq]
			if exists {
				if retB == "" {
					retB = fmt.Sprint(freq)
				}
			} else {
				freqs[freq] = struct{}{}
			}
		}

		if retA == "" {
			retA = fmt.Sprint(freq)
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
