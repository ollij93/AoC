import cfgclasses
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable
from . import (
    day1,
    day2,
    day3,
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
    # Register new days solutions here...
]


@dataclass
class Config:
    example: bool = cfgclasses.arg("Use the example data, not the real data.")

    def run(self) -> None:
        for day in ALL_DAYS:
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


if __name__ == '__main__':
    cfg = cfgclasses.parse_args(Config, sys.argv[1:], "2024")
    cfg.run()