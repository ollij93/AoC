package main

import (
	"log/slog"
	"strconv"
	"strings"
)

type Day2Solution struct{}

func (*Day2Solution) Solve(inp string) (int, int) {
	puzzle := parse2(inp)
	return puzzle.solve()
}

type idRange struct {
	start uint
	end   uint
}

func power(i uint) uint {
	ret := uint(0)
	for i > 0 {
		ret += 1
		i /= 10
	}
	return ret
}

type day2Puzzle struct {
	ranges []idRange
}

func (puzz *day2Puzzle) solve() (int, int) {
	retA := 0
	retB := 0
	for i := range puzz.ranges {
		rnge := puzz.ranges[i]
		for x := rnge.start; x <= rnge.end; x++ {
			p := power(x)
			hp := p / 2
			factor := uint(1)
			for range hp {
				factor *= 10
			}
			if x/factor == x%factor {
				retA += int(x)
				retB += int(x)
				slog.Debug("VALID:", "x", x)
				continue
			}

			// Need to check for 1..hp if all sub-nums are the same and add (and escape) if so
			for ip := uint(10); ip <= factor; ip *= 10 {
				valid := true
				for rem := x / ip; rem > 0; rem /= ip {
					if rem%ip != x%ip {
						valid = false
						break
					}
				}
				if valid {
					if power(x%ip) < power(ip)-1 {
						slog.Debug("SHORT!", "x", x, "ip", ip)
						break
					}

					slog.Debug("VALID:", "x", x)
					retB += int(x)
					break
				}
			}
		}
	}
	return retA, retB
}

func parse2(inp string) day2Puzzle {
	cleaninp := strings.TrimSpace(inp)
	segments := strings.Split(cleaninp, ",")
	ranges := make([]idRange, len(segments))
	for i := range segments {
		str := segments[i]
		parts := strings.Split(str, "-")
		a, _ := strconv.Atoi(parts[0])
		b, _ := strconv.Atoi(parts[1])
		ranges[i] = idRange{start: uint(a), end: uint(b)}
	}
	return day2Puzzle{ranges}
}
