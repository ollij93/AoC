package main

import (
	"fmt"
	"log/slog"
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

	if os.Getenv("DEBUG") == "1" {
		slog.SetLogLoggerLevel(slog.LevelDebug)
	}

	day := Day[int]{
		Num:      1,
		Solution: &Day1Solution{},
	}
	day.Run(datadir)

	day = Day[int]{
		Num:      2,
		Solution: &Day2Solution{},
	}
	day.Run(datadir)

	day = Day[int]{
		Num:      3,
		Solution: &Day3Solution{},
	}
	day.Run(datadir)

	day = Day[int]{
		Num:      4,
		Solution: &Day4Solution{},
	}
	day.Run(datadir)

	day = Day[int]{
		Num:      5,
		Solution: &Day5Solution{},
	}
	day.Run(datadir)

	dayu := Day[uint]{
		Num:      6,
		Solution: &Day6Solution{},
	}
	dayu.Run(datadir)

	dayu = Day[uint]{
		Num:      7,
		Solution: &Day7Solution{},
	}
	dayu.Run(datadir)

	dayu = Day[uint]{
		Num:      8,
		Solution: &Day8Solution{},
	}
	dayu.Run(datadir)

	dayu = Day[uint]{
		Num:      9,
		Solution: &Day9Solution{},
	}
	dayu.Run(datadir)

	dayu = Day[uint]{
		Num:      10,
		Solution: &Day10Solution{},
	}
	dayu.Run(datadir)
}
