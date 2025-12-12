package main

import (
	"strings"
)

type Day11Solution struct{}

func (*Day11Solution) Solve(inp string) (uint, uint) {
	puzzle := parse11(inp)
	return puzzle.solve()
}

type day11Puzzle struct {
	links map[string]map[string]struct{}
}

func (puzz *day11Puzzle) trackPaths(from string, to string, ch chan uint) {
	// Sends a count of paths to the channel each time "to" is reached
	// Map of points to count of ways to get there
	at := make(map[string]uint)
	// Start at "from"
	at[from] = uint(1)

	for len(at) > 0 {
		newat := make(map[string]uint)
		for oldpoint := range at {
			for newpoint := range puzz.links[oldpoint] {
				if newpoint == to {
					ch <- at[oldpoint]
				} else {
					if _, exists := newat[newpoint]; !exists {
						newat[newpoint] = 0
					}
					newat[newpoint] += at[oldpoint]
				}
			}
		}
		at = newat
	}
	close(ch)
}

func (puzz *day11Puzzle) solveA() (ret uint) {
	ch := make(chan uint)
	go puzz.trackPaths("you", "out", ch)
	for n := range ch {
		ret += n
	}
	return
}
func (puzz *day11Puzzle) solveB() (ret uint) {
	// Assuming there's no loops then we can break this up
	// A: svr -> fft
	// B: fft -> dac
	// C: dac -> out
	// Don't need dac -> fft considerations as this would produce loops so can't
	// exist
	ch := make(chan uint)
	A := uint(0)
	go puzz.trackPaths("svr", "fft", ch)
	for n := range ch {
		A += n
	}

	ch = make(chan uint)
	B := uint(0)
	go puzz.trackPaths("fft", "dac", ch)
	for n := range ch {
		B += n
	}

	ch = make(chan uint)
	C := uint(0)
	go puzz.trackPaths("dac", "out", ch)
	for n := range ch {
		C += n
	}
	return A * B * C
}
func (puzz *day11Puzzle) solve() (uint, uint) {
	return puzz.solveA(), puzz.solveB()
}

func parse11(inp string) day11Puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	links := make(map[string]map[string]struct{})
	for _, line := range lines {
		parts := strings.Fields(line)
		key := parts[0][:len(parts[0])-1]
		others := parts[1:]
		keylinks := make(map[string]struct{})
		for _, other := range others {
			keylinks[other] = struct{}{}
		}
		links[key] = keylinks
	}
	return day11Puzzle{links}
}
