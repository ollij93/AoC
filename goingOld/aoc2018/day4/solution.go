package day4

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
)

type Solution struct{}

func (*Solution) Solve(inp string) (string, string) {
	puzzle := parse(inp)
	return puzzle.solve()
}

type sleepPeriod struct {
	startTime uint
	endTime   uint
}

type schedule struct {
	sleeps []sleepPeriod
}

func totalSleepTime(schedules []schedule) (ret uint) {
	for _, sch := range schedules {
		for _, sleep := range sch.sleeps {
			ret += sleep.endTime - sleep.startTime
		}
	}
	return
}

func mostCommonMinute(schedules []schedule) (minute, count uint) {
	counts := [60]uint{}
	for _, schedule := range schedules {
		for _, sleep := range schedule.sleeps {
			for i := sleep.startTime; i < sleep.endTime; i++ {
				counts[i]++
			}
		}
	}
	for i, v := range counts {
		if v > count {
			minute = uint(i)
			count = v
		}
	}
	return
}

type puzzle struct {
	schedules map[uint][]schedule
}

func (puzz *puzzle) solve() (retA, retB string) {
	maxSeen := uint(0)
	guardNum := uint(0)
	bestMinute := uint(0)
	for guard := range puzz.schedules {
		sleepTime := totalSleepTime(puzz.schedules[guard])
		if sleepTime > maxSeen {
			minute, _ := mostCommonMinute(puzz.schedules[guard])
			maxSeen = sleepTime
			bestMinute = minute
			guardNum = guard
		}
	}
	retA = fmt.Sprint(guardNum * bestMinute)

	maxSeen = uint(0)
	guardNum = uint(0)
	bestMinute = uint(0)
	for guard := range puzz.schedules {
		minute, count := mostCommonMinute(puzz.schedules[guard])
		if count > maxSeen {
			maxSeen = count
			bestMinute = minute
			guardNum = guard
		}
	}
	retB = fmt.Sprint(guardNum * bestMinute)
	return
}

func parse(inp string) puzzle {
	cleaninp := strings.TrimSpace(inp)
	lines := strings.Split(cleaninp, "\n")
	schedules := make(map[uint][]schedule)
	sort.Strings(lines)
	curr_guard := uint(0)
	curr_schedule := schedule{}
	for _, line := range lines {
		if strings.HasSuffix(line, "begins shift") {
			if len(curr_schedule.sleeps) > 0 {
				schedules[curr_guard] = append(schedules[curr_guard], curr_schedule)
			}
			guard_num, _ := strconv.Atoi(strings.Fields(line)[3][1:])
			curr_guard = uint(guard_num)
			curr_schedule = schedule{}
			continue
		}
		// Always after midnight and before 1am, so safe to just treat as minute
		// uints
		minute, _ := strconv.Atoi(line[15:17])
		if strings.HasSuffix(line, "falls asleep") {
			curr_schedule.sleeps = append(curr_schedule.sleeps, sleepPeriod{
				uint(minute),
				uint(0),
			})
			continue
		}
		if strings.HasSuffix(line, "wakes up") {
			curr_schedule.sleeps[len(curr_schedule.sleeps)-1].endTime = uint(minute)
			continue
		}
	}
	schedules[curr_guard] = append(schedules[curr_guard], curr_schedule)
	return puzzle{schedules}
}
