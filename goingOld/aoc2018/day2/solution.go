package day2

import (
	"fmt"
	"strings"
)

type Solution struct{}

func (*Solution) Solve(inp string) (string, string) {
	puzzle := parse(inp)
	return puzzle.solve()
}

type puzzle struct {
	ids []string
}

func (puzz *puzzle) solve() (retA string, retB string) {
	numTwice := 0
	numThrice := 0
	for _, id := range puzz.ids {
		twiceFound := false
		thriceFound := false
		for _, c := range strings.Split(id, "") {
			// This is the first instance so check the count - this avoids
			// duplicate counting
			n := strings.Count(id, c)
			if n == 2 && !twiceFound {
				numTwice += 1
				twiceFound = true
			} else if n == 3 && !thriceFound {
				numThrice += 1
				thriceFound = true
			}
		}
	}
	checksum := numTwice * numThrice
	retA = fmt.Sprint(checksum)

	for i1 := 0; i1 < len(puzz.ids) && retB == ""; i1++ {
		id1 := puzz.ids[i1]
		for i2 := i1 + 1; i2 < len(puzz.ids) && retB == ""; i2++ {
			id2 := puzz.ids[i2]
			for i := range id1 {
				if id1[:i] == id2[:i] && id1[i+1:] == id2[i+1:] {
					retB = id1[:i] + id1[i+1:]
					break
				}
			}
		}
	}
	return
}

func parse(inp string) puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	return puzzle{lines}
}
