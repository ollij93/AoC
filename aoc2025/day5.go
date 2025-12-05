package main

import (
	"log/slog"
	"sort"
	"strconv"
	"strings"
)

type Day5Solution struct{}

func (*Day5Solution) Solve(inp string) (int, int) {
	puzzle := parse5(inp)
	return puzzle.solve()
}

type ingredRange struct {
	start uint
	end   uint
}

type day5Puzzle struct {
	ranges  []ingredRange
	ingreds []uint
}

func inRange(ingred uint, rnge ingredRange) bool {
	return (ingred >= rnge.start && ingred <= rnge.end)
}

func (puzz *day5Puzzle) validCount() (ret uint) {
	for _, ingred := range puzz.ingreds {
		for _, rnge := range puzz.ranges {
			if inRange(ingred, rnge) {
				ret += 1
				break
			}
		}
	}
	return
}

func (puzz *day5Puzzle) totalValid() (ret uint) {
	for _, rnge := range puzz.ranges {
		num := rnge.end - rnge.start + 1
		slog.Debug("Range", "start", rnge.start, "end", rnge.end, "num", num)
		ret += num
	}
	return
}

func (puzz *day5Puzzle) solve() (int, int) {
	retA := puzz.validCount()
	retB := puzz.totalValid()
	return int(retA), int(retB)
}

func parse5ranges(inp string) []ingredRange {
	lines := strings.Split(inp, "\n")
	ret := make([]ingredRange, len(lines))
	for i, line := range lines {
		lineparts := strings.Split(line, "-")
		start, _ := strconv.Atoi(lineparts[0])
		end, _ := strconv.Atoi(lineparts[1])
		ret[i] = ingredRange{uint(start), uint(end)}
	}
	return ret
}

func removeOverlaps(ranges []ingredRange) []ingredRange {
	for i1 := range ranges {
		for i2 := i1 + 1; i2 < len(ranges); i2++ {
			range1 := &ranges[i1]
			range2 := &ranges[i2]
			if range1.end >= range2.start {
				// Overlapping
				// Handle range2 fully in range1
				range2end := range2.end
				if range1.end > range2.end {
					range2end = range1.end
				}
				if range1.start == range2.start {
					// Both start on the same number, so bump range2 up by one so the ranges never become zero length
					range2.start += 1
				}
				// Update the two ranges to remove the overlap
				range1.end = range2.start - 1
				range2.end = range2end
				slog.Debug("Rewrote", "i1", i1, "i2", i2, "range1", range1, "range2", range2)
			}
		}
	}
	return ranges
}

func parse5(inp string) day5Puzzle {
	cleaninp := strings.TrimSpace(inp)
	parts := strings.Split(cleaninp, "\n\n")
	ranges := parse5ranges(parts[0])
	sort.Slice(ranges[:], func(i, j int) bool {
		if ranges[i].start == ranges[j].start {
			return ranges[i].end < ranges[j].end
		}
		return ranges[i].start < ranges[j].start
	})
	ranges = removeOverlaps(ranges)
	lines := strings.Split(parts[1], "\n")
	ingreds := make([]uint, len(lines))
	for i, line := range lines {
		n, _ := strconv.Atoi(line)
		ingreds[i] = uint(n)
	}
	return day5Puzzle{ranges, ingreds}
}
