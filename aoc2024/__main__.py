import cfgclasses
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable
from . import (
    day1,
    day2,
    day3,
    day4,
    day5,
    day6,
    day7,
    day8,
    day9,
    day10,
    # Import new days solutions here...
)


@dataclass
class Day:
    name: str
    p1: Callable[[str], Any]
    p1_answer: Any
    p2: Callable[[str], Any]
    p2_answer: Any

    @property
    def data_dir(self) -> Path:
        return Path(__file__).parent / "data" / self.name


ALL_DAYS = [
    Day("day1", day1.p1, 11, day1.p2, 31),
    Day("day2", day2.p1, 2, day2.p2, 4),
    Day("day3", day3.p1, 161, day3.p2, 48),
    Day("day4", day4.p1, 18, day4.p2, 9),
    Day("day5", day5.p1, 143, day5.p2, 123),
    Day("day6", day6.p1, 41, day6.p2, 6),
    Day("day7", day7.p1, 3749, day7.p2, 11387),
    Day("day8", day8.p1, 14, day8.p2, 34),
    Day("day9", day9.p1, 1928, day9.p2, 2858),
    Day("day10", day10.p1, 36, day10.p2, 81),
    # Register new days solutions here...
]


@dataclass
class Config:
    example: bool = cfgclasses.arg("Use the example data, not the real data.")
    day: int = cfgclasses.arg("Specific day to run the process for", default=-1)

    def run_day(self, day: Day) -> None:
        data_file = day.data_dir / ("example.txt" if self.example else "real.txt")
        data = data_file.read_text()
        print(f"== {day.name} ==")
        try:
            p1 = day.p1(data)
        except NotImplementedError:
            p1 = "NotImplemented"
        print("P1:", p1)
        try:
            p2 = day.p2(data)
        except NotImplementedError:
            p2 = "NotImplemented"
        print("P2:", p2)
        print()

    def run(self) -> None:
        if self.day == -1:
            for day in ALL_DAYS:
                self.run_day(day)
        else:
            self.run_day(ALL_DAYS[self.day - 1])


if __name__ == "__main__":
    cfg = cfgclasses.parse_args(Config, sys.argv[1:], "2024")
    cfg.run()
