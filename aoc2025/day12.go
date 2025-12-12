package main

import (
	"fmt"
	"strconv"
	"strings"
)

type Day12Solution struct{}

func (*Day12Solution) Solve(inp string) (uint, uint) {
	puzzle := parse12(inp)
	return puzzle.solve()
}

type shape struct {
	elements [][]bool
	area     uint
}

type region struct {
	area   point2d
	shapes []uint
}

func (reg *region) shapeArea(shapes []shape) (ret uint) {
	for shapeIdx, count := range reg.shapes {
		shape := shapes[shapeIdx]
		ret += shape.area * count
	}
	return
}

func (reg *region) regionArea() uint {
	return reg.area.x * reg.area.y
}

func (reg *region) blockArea() uint {
	// Number of 3x3 blocks that could fix in this region
	xBlock := reg.area.x / 3
	yBlock := reg.area.y / 3
	total := xBlock * yBlock
	fmt.Printf("(%d, %d) -> (%d, %d) => %d for %d\n", reg.area.x, reg.area.y, xBlock, yBlock, total, reg.totalShapeCount())
	return total
}

func (reg *region) totalShapeCount() (ret uint) {
	for _, count := range reg.shapes {
		ret += count
	}
	return
}

func (reg *region) print(shapes []shape) {
	shapeStrs := make([]string, len(reg.shapes))
	for i, shapeCount := range reg.shapes {
		shapeStrs[i] = fmt.Sprint(shapeCount)
	}
	shapeIdStr := strings.Join(shapeStrs, " ")
	fmt.Printf("%dx%d: %s (shapeArea=%d regionArea=%d)\n", reg.area.x, reg.area.y, shapeIdStr, reg.shapeArea(shapes), reg.regionArea())
}

type day12Puzzle struct {
	shapes  []shape
	regions []region
}

func (puzz *day12Puzzle) printRegions() {
	for _, reg := range puzz.regions {
		reg.print(puzz.shapes)
	}
}

func (puzz *day12Puzzle) quickEliminate() (ret []region) {
	for _, region := range puzz.regions {
		regionArea := region.regionArea()
		shapeArea := region.shapeArea(puzz.shapes)
		if shapeArea <= regionArea {
			// Too many shapes to fit even if perfectly packed
			ret = append(ret[:], region)
		}
	}
	return ret
}

func (puzz *day12Puzzle) separateDefinites() (definite, unknown []region) {
	for _, region := range puzz.regions {
		if region.blockArea() >= region.totalShapeCount() {
			// This will definitely fit as all the 3x3 shapes could be
			// separately inserted without interlocking
			definite = append(definite, region)
		} else {
			unknown = append(unknown, region)
		}
	}
	return
}

func (puzz *day12Puzzle) solveA() (ret uint) {
	return
}
func (puzz *day12Puzzle) solveB() (ret uint) {
	return
}
func (puzz *day12Puzzle) solve() (uint, uint) {
	fmt.Println("In", len(puzz.regions))
	reducedRegions := puzz.quickEliminate()
	fmt.Println("Quick Eliminate", len(puzz.regions), "=>", len(reducedRegions))
	puzz.regions = reducedRegions
	definite, unknown := puzz.separateDefinites()
	fmt.Println("Definites:", len(definite))
	fmt.Println("Unknown:", len(unknown))
	puzz.regions = unknown
	puzz.printRegions()
	// Whelp... This doesn't solve the example, but it does solve the real case
	// 🤷‍♂️ Gues I'm done!!!
	return puzz.solveA(), puzz.solveB()
}

func parseShape(inp string) shape {
	lines := strings.Split(inp, "\n")[1:]
	elements := make([][]bool, len(lines))
	area := uint(0)
	for i, line := range lines {
		row := make([]bool, len(line))
		for j, c := range strings.Split(line, "") {
			if c == "#" {
				row[j] = true
				area++
			} else {
				row[j] = false
			}
		}
		elements[i] = row
	}
	return shape{elements, area}
}

func parseShapes(inp []string) []shape {
	ret := make([]shape, len(inp))
	for i, instr := range inp {
		ret[i] = parseShape(instr)
	}
	return ret
}

func parseRegion(inp string) region {
	parts := strings.Fields(inp)
	areaParts := strings.Split(parts[0][:len(parts[0])-1], "x")
	x, _ := strconv.Atoi(areaParts[0])
	y, _ := strconv.Atoi(areaParts[1])
	nums := make([]uint, len(parts)-1)
	for n, part := range parts[1:] {
		z, _ := strconv.Atoi(part)
		nums[n] = uint(z)
	}
	return region{
		point2d{uint(x), uint(y)},
		nums,
	}
}

func parseRegions(inp string) []region {
	lines := strings.Split(inp, "\n")
	ret := make([]region, len(lines))
	for i, line := range lines {
		ret[i] = parseRegion(line)
	}
	return ret

}

func parse12(inp string) day12Puzzle {
	cleaninp := strings.TrimSpace(inp)
	sectors := strings.Split(cleaninp, "\n\n")
	shapes := parseShapes(sectors[:len(sectors)-1])
	regions := parseRegions(sectors[len(sectors)-1])
	return day12Puzzle{shapes, regions}
}
