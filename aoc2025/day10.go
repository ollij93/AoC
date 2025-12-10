package main

import (
	"fmt"
	"strconv"
	"strings"
)

type Day10Solution struct{}

func (*Day10Solution) Solve(inp string) (uint, uint) {
	puzzle := parse10(inp)
	return puzzle.solve()
}

type button struct {
	values []uint
}

type machine struct {
	targetLights [10]bool
	buttons      []*button
	joltages     [10]uint
}

func (mach *machine) solveJoltage(i int, ch chan uint) {
	states := make(map[[10]uint]struct{})
	// Initial state of "all zero"
	states[[10]uint{}] = struct{}{}
	iter := uint(0)
	for {
		iter++
		newStates := make(map[[10]uint]struct{})
		for _, button := range mach.buttons {
			if button == nil {
				break
			}
			for state := range states {
				newState := [10]uint{}
				invalid := false
				copy(newState[:], state[:])
				for _, n := range button.values {
					newState[n] += 1
					if newState[n] > mach.joltages[n] {
						invalid = true
						break
					}
				}
				if !invalid {
					newStates[newState] = struct{}{}
				}
			}
		}
		states = newStates
		//fmt.Println(i, "::", iter, "N states", len(states))
		_, targetFound := states[mach.joltages]
		if targetFound {
			fmt.Println(i, "Done")
			ch <- iter
			break
		}
	}
}

type day10Puzzle struct {
	machs []machine
}

func (puzz *day10Puzzle) solveA() (ret uint) {
	for _, mach := range puzz.machs {
		states := make(map[[10]bool]struct{})
		// Initial state of "all off"
		states[[10]bool{}] = struct{}{}
		iter := uint(0)
		for {
			iter++
			newStates := make(map[[10]bool]struct{})
			for _, button := range mach.buttons {
				if button == nil {
					break
				}
				for state := range states {
					newState := [10]bool{}
					copy(newState[:], state[:])
					for _, n := range button.values {
						newState[n] = !newState[n]
					}
					newStates[newState] = struct{}{}
				}
			}
			states = newStates
			_, targetFound := states[mach.targetLights]
			if targetFound {
				ret += iter
				break
			}
		}
	}
	return
}
func (puzz *day10Puzzle) solveB() (ret uint) {
	ch := make(chan uint)
	for i, mach := range puzz.machs {
		go mach.solveJoltage(i, ch)
	}
	for i := range puzz.machs {
		ret += <-ch
		fmt.Println("RET", ret, "remaining", len(puzz.machs)-i)
	}
	//	for i, ch := range chs {
	//		val := <-ch
	//		fmt.Println("Done with ", i, ":", val)
	//		ret += val
	//	}
	return
}
func (puzz *day10Puzzle) solve() (uint, uint) {
	return puzz.solveA(), puzz.solveB()
}

func parse10Lights(inp string) (ret [10]bool) {
	inp = inp[1 : len(inp)-1]
	for i, c := range strings.Split(inp, "") {
		ret[i] = c == "#"
	}
	return
}

func parse10Buttons(inp []string) []*button {
	ret := make([]*button, len(inp))
	for i, part := range inp {
		part = part[1 : len(part)-1]
		nums := strings.Split(part, ",")
		buttonVals := make([]uint, len(nums))
		for n, nstr := range nums {
			val, _ := strconv.Atoi(nstr)
			buttonVals[n] = uint(val)
		}
		ret[i] = &button{buttonVals}
	}
	return ret
}

func parse10Joltages(inp string) (ret [10]uint) {
	inp = inp[1 : len(inp)-1]
	parts := strings.Split(inp, ",")
	for i, part := range parts {
		val, _ := strconv.Atoi(part)
		ret[i] = uint(val)
	}
	return
}

func parse10(inp string) day10Puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	machs := make([]machine, len(lines))
	for i, line := range lines {
		parts := strings.Fields(line)
		lights := parse10Lights(parts[0])
		joltages := parse10Joltages(parts[len(parts)-1])
		buttons := parse10Buttons(parts[1 : len(parts)-1])
		machs[i] = machine{
			lights,
			buttons,
			joltages,
		}
	}
	return day10Puzzle{machs}
}
