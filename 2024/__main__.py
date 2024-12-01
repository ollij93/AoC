import cfgclasses
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable
from . import day1


@dataclass
class Day:
    name: str
    p1: Callable[[str], Any]
    p2: Callable[[str], Any]


ALL_DAYS = [Day("day1", day1.p1, day1.p2)]


@dataclass
class Config:
    example: bool = cfgclasses.arg("Use the example data, not the real data.")

    def run(self) -> None:
        for day in ALL_DAYS:
            data_dir = Path(__file__).parent / "data" / day.name
            data_file = data_dir / ("example.txt" if self.example else "real.txt")
            data = data_file.read_text()
            print(f"== {day.name} ==")
            print("P1:", day.p1(data))
            print("P2:", day.p2(data))
            print()


cfg = cfgclasses.parse_args(Config, sys.argv[1:], "2024")
cfg.run()
