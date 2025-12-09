package aoc2018

import (
	"fmt"
	"goingOld/aoc2018/day1"
	"goingOld/aoc2018/day2"
	"goingOld/aoc2018/day3"
	"os"
	"path/filepath"
)

type Solution interface {
	Solve(string) (string, string)
}

type Day struct {
	Num      uint
	Solution Solution
}

func (day *Day) data(datadir string, name string) string {
	file := filepath.Join(datadir, fmt.Sprintf("day%d", day.Num), name)
	rawdata, _ := os.ReadFile(file)
	data := string(rawdata)
	return data
}

func (day *Day) ExampleData(datadir string) string {
	return day.data(datadir, "example.txt")
}

func (day *Day) RealData(datadir string) string {
	return day.data(datadir, "real.txt")
}

func Run() {
	cwd, _ := os.Getwd()
	datadir := filepath.Join(cwd, "aoc2018")

	days := []Day{
		{1, &day1.Solution{}},
		{2, &day2.Solution{}},
		{3, &day3.Solution{}},
	}

	for _, day := range days {
		fmt.Println("=== Day", day.Num, "===")
		example_data := day.ExampleData(datadir)
		a, b := day.Solution.Solve(example_data)
		fmt.Println("EXAMPLE:", a, b)

		real_data := day.RealData(datadir)
		a, b = day.Solution.Solve(real_data)
		fmt.Println("REAL:", a, b)
	}
}
