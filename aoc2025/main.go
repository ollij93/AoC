package main

import (
	"fmt"
	"os"
	"path/filepath"
)

type Solution[T any] interface {
	Solve(string) (T, T)
}

type Day[T any] struct {
	Num      uint
	Solution Solution[T]
}

func (day *Day[T]) Run(datadir string) {
	fmt.Println("==== Running day", day.Num, "====")

	file := filepath.Join(datadir, fmt.Sprintf("day%d", day.Num), "example.txt")
	rawinput, _ := os.ReadFile(file)
	input := string(rawinput)
	a, b := day.Solution.Solve(input)
	fmt.Println("EXAMPLE:", a, b)

	file = filepath.Join(datadir, fmt.Sprintf("day%d", day.Num), "real.txt")
	rawinput, _ = os.ReadFile(file)
	input = string(rawinput)
	a, b = day.Solution.Solve(input)
	fmt.Println("REAL:", a, b)
	fmt.Println()
}

func main() {
	cwd, _ := os.Getwd()
	datadir := filepath.Join(cwd, "data")

	day1 := Day[int]{
		Num:      1,
		Solution: &Day1Solution{},
	}
	day1.Run(datadir)
}
