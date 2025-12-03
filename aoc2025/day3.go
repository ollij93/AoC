package main

import (
	"log/slog"
	"strings"
)

type Day3Solution struct{}

func (*Day3Solution) Solve(inp string) (int, int) {
	puzzle := parse3(inp)
	return puzzle.solve()
}

type batteryBank struct {
	batts []uint
}

func maxJoltage(batts []uint, from uint) uint {
	if from == 0 {
		return 0
	}
	bstTen := batts[0]
	bstSub := batts[1:]
	remainingBatts := len(batts)
	neededForSub := int(from) - 1
	toPickFrom := remainingBatts - neededForSub
	slog.Debug("Getting max joltage",
		"from", batts,
		"remainingBatts", remainingBatts,
		"neededForSub", neededForSub,
		"toPickFrom", toPickFrom,
	)
	for i, val := range batts[:toPickFrom] {
		sub := batts[i+1:]
		if val > bstTen {
			bstTen = val
			bstSub = sub
		}
	}
	base := maxJoltage(bstSub, from-1)
	for range from - 1 {
		bstTen *= 10
	}
	return bstTen + base
}

func (bank *batteryBank) maxJoltage(from uint) uint {
	return maxJoltage(bank.batts, from)
}

type day3Puzzle struct {
	banks []batteryBank
}

func (puzz *day3Puzzle) solve() (int, int) {
	retA := 0
	for _, bank := range puzz.banks {
		max := bank.maxJoltage(2)
		retA += int(max)
		slog.Debug("Joltage (2)", "bank", bank.batts, "max", max)
	}
	retB := 0
	for _, bank := range puzz.banks {
		max := bank.maxJoltage(12)
		retB += int(max)
		slog.Debug("Joltage (12)", "bank", bank.batts, "max", max)
	}
	return retA, retB
}

func parse3(inp string) day3Puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	banks := make([]batteryBank, len(lines))
	for i, line := range lines {
		nums := make([]uint, len(line))
		for i, c := range line {
			nums[i] = uint(c) - uint('0')
		}
		banks[i] = batteryBank{batts: nums}
	}
	return day3Puzzle{banks}
}
