package main

import (
	"log/slog"
	"sort"
	"strconv"
	"strings"
)

type Day8Solution struct{}

func (*Day8Solution) Solve(inp string) (uint, uint) {
	puzzle := parse8(inp)
	return puzzle.solve()
}

type point3d struct {
	x uint
	y uint
	z uint
}

func (p *point3d) distSqrd(other *point3d) uint {
	return (p.x-other.x)*(p.x-other.x) + (p.y-other.y)*(p.y-other.y) + (p.z-other.z)*(p.z-other.z)
}

type day8Puzzle struct {
	points []point3d
}

// Distance list, each entry details the distance between two points
// The node also contains pointers to other nodes:
// A the "next" known bigger node.
// B an unsorted list of smaller distance nodes. These values will have "next" set to a value which may be bigger or smaller.
type distList struct {
	dist     uint
	fromi    uint
	toi      uint
	next     *distList // If not nil, this points to the next biggest element, which may or may not have been sorted
	unsorted *distList // If not nil, this points to more (unsorted) entries which are known to have *smaller* values than this node
}

// Get the next item numerically in the list, running the sorting process if needed
func (list *distList) nextSorted() *distList {
	slog.Debug("Getting next sorted from", "curr", list.dist)
	// We're only considering nodes "bigger" than the current, so ignore ourselves and unsorted.
	if list.next == nil {
		return nil
	}
	if list.next.unsorted != nil {
		newList := &distList{
			list.next.dist,
			list.next.fromi,
			list.next.toi,
			nil,
			nil,
		}
		back := newList
		// The next node is unsorted. Sort it by re-inserting all the nodes to form a new "list".
		for node := list.next.unsorted; node != nil; {
			next := node.next
			node.next = nil
			node.unsorted = nil
			newList = newList.insert(node, true)
			node = next
		}
		back.next = list.next.next
		list.next = newList
	}
	// Next node now guaranteed to be sorted
	return list.next
}

func (list *distList) insert(point *distList, sort bool) *distList {
	// Returns the pointer to the new head of the list
	slog.Debug("Inserting new", "from", point.fromi, "to", point.toi, "dist", point.dist)
	slog.Debug("  prev", "from", list.fromi, "to", list.toi, "dist", list.dist)
	if list.dist > point.dist {
		// Point is smaller than the current list entry
		// If sorting, use this as the new head, otherwise put it in the unsorted bucket
		if sort {
			slog.Debug("Inserted as new head")
			point.next = list
			return point
		} else {
			slog.Debug("Inserted into unsorted")
			point.next = list.unsorted
			list.unsorted = point
			return list
		}
	} else if list.next == nil {
		slog.Debug("Inserted at end (new biggest)")
		list.next = point
		point.next = nil
		return list
	} else {
		slog.Debug("Inserting further into list")
		list.next = list.next.insert(point, false) // Don't sort deeper than we need to go now
		return list
	}
}

func (puzz *day8Puzzle) processStep(
	topList *distList,
	pointToGroup map[uint]int,
	groupToPoints map[int]map[uint]struct{},
	groupsCount int,
	liveGroupsCount int,
) (*distList, int, int) {
	link := topList
	topList = topList.nextSorted()
	pointA := link.fromi
	pointB := link.toi
	groupA, okA := pointToGroup[pointA]
	groupB, okB := pointToGroup[pointB]
	if okA {
		if okB {
			// Both points already in groups
			if groupA == groupB {
				// Both points already in the same group
			} else {
				// Both points in different groups - merge
				allBPoints := groupToPoints[groupB]
				// Reassign all points from B to A
				for point := range allBPoints {
					groupToPoints[groupA][point] = struct{}{}
					pointToGroup[point] = groupA
				}
				// Empty the groupB map
				groupToPoints[groupB] = make(map[uint]struct{})
				liveGroupsCount -= 1
			}
		} else {
			// A in a group, B not, so put B in groupA
			pointToGroup[pointB] = groupA
			groupToPoints[groupA][pointB] = struct{}{}
		}
	} else {
		if okB {
			// B in a group, A not, so put A in groupB
			pointToGroup[pointA] = groupB
			groupToPoints[groupB][pointA] = struct{}{}
		} else {
			// Neither in a group, so make a new group and add both
			newGroup := groupsCount
			pointToGroup[pointA] = newGroup
			pointToGroup[pointB] = newGroup
			groupToPoints[newGroup] = make(map[uint]struct{})
			groupToPoints[newGroup][pointA] = struct{}{}
			groupToPoints[newGroup][pointB] = struct{}{}
			groupsCount += 1
			liveGroupsCount += 1
		}
	}
	return topList, groupsCount, liveGroupsCount
}

func (puzz *day8Puzzle) solve() (retA, retB uint) {
	// Find the 10/1000 closest distSqrds (decide based on len of input)
	wanted := 1000
	if len(puzz.points) < 200 {
		wanted = 10
	}

	// Insert a really far away point at the end of the list so that its always
	// put in as the "biggest" distance after we've done one pass of distances
	// This avoids the tail end of the list getting prematurely sorted
	//puzz.points = append(puzz.points, point3d{999999, 999999, 999999})

	var topList *distList
	for i1, p1 := range puzz.points {
		for i2 := i1 + 1; i2 < len(puzz.points); i2++ {
			p2 := puzz.points[i2]
			dist := p1.distSqrd(&p2)
			newPoint := distList{dist, uint(i1), uint(i2), nil, nil}
			if topList == nil {
				topList = &newPoint
			} else {
				topList = topList.insert(&newPoint, true)
			}
		}
	}

	// Create a map of pointId to "group id" and the reverse map of "group id" to pointId
	// Then when we find A and B are connected we can merge their groups
	// Use ints for groups to avoid typing confusion
	groupsCount := 0
	pointToGroup := make(map[uint]int)
	groupToPoints := make(map[int]map[uint]struct{})
	liveGroupsCount := 0
	for range wanted {
		topList, groupsCount, liveGroupsCount = puzz.processStep(topList, pointToGroup, groupToPoints, groupsCount, liveGroupsCount)
	}
	// Get the length of all the groups and sort
	lens := make([]int, len(groupToPoints))
	for groupId := range groupToPoints {
		lens[groupId] = len(groupToPoints[groupId])
	}
	sort.Ints(lens)
	// Take the longest three
	a := lens[len(lens)-1]
	b := lens[len(lens)-2]
	c := lens[len(lens)-3]
	retA = uint(a * b * c)

	// Continue processing until we only have one live group
	var mostRecentLink *distList
	// Sub 1 for special added point
	for liveGroupsCount > 1 || len(pointToGroup) < len(puzz.points) {
		mostRecentLink = topList
		topList, groupsCount, liveGroupsCount = puzz.processStep(topList, pointToGroup, groupToPoints, groupsCount, liveGroupsCount)
	}
	retB = uint(puzz.points[mostRecentLink.fromi].x * puzz.points[mostRecentLink.toi].x)
	return
}

func parse8(inp string) day8Puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	points := make([]point3d, len(lines))
	for i, line := range lines {
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		z, _ := strconv.Atoi(parts[2])
		points[i] = point3d{uint(x), uint(y), uint(z)}
	}
	return day8Puzzle{points}
}
