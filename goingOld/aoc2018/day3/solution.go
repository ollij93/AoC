package day3

import (
	"fmt"
	"strconv"
	"strings"
)

type Solution struct{}

func (*Solution) Solve(inp string) (string, string) {
	puzzle := parse(inp)
	return puzzle.solve()
}

type point2d struct {
	x uint
	y uint
}

type area struct {
	point point2d
	dims  point2d
}

type claim struct {
	id     uint
	region area
}

type puzzle struct {
	claims []claim
}

func (puzz *puzzle) solve() (retA, retB string) {
	// Build up a set of points that's easy to count later
	points := make(map[uint]map[uint]struct{})
	for i1 := 0; i1 < len(puzz.claims); i1++ {
		c1 := puzz.claims[i1]
		a1 := c1.region
		anyOverlapped := false
		// For partB its easier to re-run a bunch of comparisons here, so just
		// avoid comparing A to A but do both A to B and B to A
		for i2 := 0; i2 < len(puzz.claims); i2++ {
			if i2 == i1 {
				continue
			}
			c2 := puzz.claims[i2]
			a2 := c2.region
			topLeft := point2d{
				max(a1.point.x, a2.point.x),
				max(a1.point.y, a2.point.y),
			}
			bottomRight := point2d{
				min(a1.point.x+a1.dims.x-1, a2.point.x+a2.dims.x-1),
				min(a1.point.y+a1.dims.y-1, a2.point.y+a2.dims.y-1),
			}
			if bottomRight.x < topLeft.x || bottomRight.y < topLeft.y {
				// Not a valid overlap
				continue
			}
			for x := topLeft.x; x <= bottomRight.x; x++ {
				submap, exists := points[x]
				if !exists {
					submap = make(map[uint]struct{})
					points[x] = submap
				}
				for y := topLeft.y; y <= bottomRight.y; y++ {
					submap[y] = struct{}{}
				}
			}
			anyOverlapped = true
		}
		if !anyOverlapped {
			retB = fmt.Sprint(c1.id)
		}
	}
	numPoints := 0
	for x := range points {
		numPoints += len(points[x])
	}
	retA = fmt.Sprint(numPoints)
	return
}

func parse(inp string) puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	claims := make([]claim, len(lines))
	for i, line := range lines {
		parts := strings.Split(line, " ")
		id, _ := strconv.Atoi(parts[0][1:])
		coords := strings.Split(parts[2][:len(parts[2])-1], ",")
		px, _ := strconv.Atoi(coords[0])
		py, _ := strconv.Atoi(coords[1])
		dims := strings.Split(parts[3], "x")
		ax, _ := strconv.Atoi(dims[0])
		ay, _ := strconv.Atoi(dims[1])
		claims[i] = claim{
			uint(id),
			area{
				point2d{uint(px), uint(py)},
				point2d{uint(ax), uint(ay)},
			},
		}
	}
	return puzzle{claims}
}
