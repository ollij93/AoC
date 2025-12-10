package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mitchellh/go-z3"
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
	joltages     []uint
}

func (mach *machine) solveJoltage(i int) uint {
	config := z3.NewConfig()
	ctx := z3.NewContext(config)
	config.Close()
	defer ctx.Close()

	s := ctx.NewSolver()
	defer s.Close()

	// Zero constant needed for comparison
	zero := ctx.Int(0, ctx.IntSort())

	conditionSums := make([]*z3.AST, len(mach.joltages))
	variables := make([]*z3.AST, len(mach.buttons))
	for i, button := range mach.buttons {
		variable := ctx.Const(ctx.Symbol(fmt.Sprint("b", i)), ctx.IntSort())
		for _, value := range button.values {
			if conditionSums[value] == nil {
				conditionSums[value] = variable
			} else {
				conditionSums[value] = conditionSums[value].Add(variable)
			}
		}
		s.Assert(variable.Ge(zero))
		variables[i] = variable
	}
	// ConditionSums is now B1 + B2 + B3 ..., just need to set the "= J" bit
	for n, condition := range conditionSums {
		if condition == nil {
			fmt.Println("NULL AT", n)
		}
		expectedVal := ctx.Int(int(mach.joltages[n]), ctx.IntSort())
		s.Assert(condition.Eq(expectedVal))
	}

	// Current solution will get us "any" solution
	// Repeatedly add more constraints on the total to get the minimum

	// Define the total
	total := ctx.Const(ctx.Symbol("total"), ctx.IntSort())
	var totalVal *z3.AST
	for i := range mach.buttons {
		if totalVal == nil {
			totalVal = variables[i]
		} else {
			totalVal = totalVal.Add(variables[i])
		}
	}
	s.Assert(total.Eq(totalVal))

	curr_min := uint(0)
	for {
		if v := s.Check(); v != z3.True {
			break
		}

		m := s.Model()
		result := m.Eval(total).Int()
		curr_min = uint(result)

		// Apply the constraint for the next loop
		s.Assert(total.Lt(ctx.Int(result, ctx.IntSort())))
		m.Close()
	}
	return curr_min
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
	for i, mach := range puzz.machs {
		ret += mach.solveJoltage(i)
	}
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

func parse10Joltages(inp string) []uint {
	inp = inp[1 : len(inp)-1]
	parts := strings.Split(inp, ",")
	ret := make([]uint, len(parts))
	for i, part := range parts {
		val, _ := strconv.Atoi(part)
		ret[i] = uint(val)
	}
	return ret
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
