package main

import (
	"strconv"
	"strings"
)

type Day9Solution struct{}

func (*Day9Solution) Solve(inp string) (uint, uint) {
	puzzle := parse9(inp)
	return puzzle.solve()
}

type point2d struct {
	x uint
	y uint
}

type day9Puzzle struct {
	points []point2d
}

func (puzz *day9Puzzle) hasBadIntersects(pnt1, pnt2 point2d) bool {
	// For part B need to exclude rectangles where:
	// A) Any other point inside the interior of the rectangle
	// B) Any line bisecting the interior of the rectangle
	// This could be flawed as we might find a rectangle entirely outside the
	// area, but lets hope and assume that's not the "largest" rectangle we'll
	// have.
	for i1 := 0; i1 < len(puzz.points); i1++ {
		p1 := puzz.points[i1]
		p1InX := (p1.x > pnt1.x && p1.x < pnt2.x) || (p1.x < pnt1.x && p1.x > pnt2.x)
		p1InY := (p1.y > pnt1.y && p1.y < pnt2.y) || (p1.y < pnt1.y && p1.y > pnt2.y)
		if p1InX && p1InY {
			// Point fully inside the area
			return true
		}
		var p2 point2d
		if i1 == len(puzz.points)-1 {
			p2 = puzz.points[0]
		} else {
			p2 = puzz.points[i1+1]
		}
		// Check if this a horizontal or vertical line
		vertical := p1.x == p2.x
		if vertical {
			if p1InX {
				// This line, if long enough, could bisect the area
				areaYMin := pnt1.y + 1
				areaYMax := pnt2.y - 1
				if pnt2.y < pnt1.y {
					areaYMin = pnt2.y + 1
					areaYMax = pnt1.y - 1
				}
				lineYMin := p1.y
				lineYMax := p2.y
				if p2.y < p1.y {
					lineYMin = p2.y
					lineYMax = p1.y
				}

				if lineYMin < areaYMin && lineYMax > areaYMax {
					// This line cuts through the area
					return true
				}
			}
		} else {
			if p1InY {
				// This line, if long enough, could bisect the area
				areaXMin := pnt1.x + 1
				areaXMax := pnt2.x - 1
				if pnt2.x < pnt1.x {
					areaXMin = pnt2.x + 1
					areaXMax = pnt1.x - 1
				}
				lineXMin := p1.x
				lineXMax := p2.x
				if p2.x < p1.x {
					lineXMin = p2.x
					lineXMax = p1.x
				}

				if lineXMin < areaXMin && lineXMax > areaXMax {
					// This line cuts through the area
					return true
				}
			}
		}
	}
	return false
}

func absDiffUint(x, y uint) uint {
	if x > y {
		return x - y
	} else {
		return y - x
	}
}

func (puzz *day9Puzzle) solve() (retA, retB uint) {
	for i1 := 0; i1 < len(puzz.points); i1++ {
		p1 := puzz.points[i1]
		for i2 := i1 + 1; i2 < len(puzz.points); i2++ {
			p2 := puzz.points[i2]
			area := (absDiffUint(p1.x, p2.x) + 1) * (absDiffUint(p1.y, p2.y) + 1)
			if area > retA {
				retA = area
			}
			if area > retB && !puzz.hasBadIntersects(p1, p2) {
				retB = area
			}
		}
	}
	return
}

func parse9(inp string) day9Puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	points := make([]point2d, len(lines))
	for i, line := range lines {
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		points[i] = point2d{uint(x), uint(y)}
	}
	return day9Puzzle{points}
}
