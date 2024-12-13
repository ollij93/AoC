from dataclasses import dataclass
from typing import Optional


@dataclass
class Puzzle:
    a: tuple[int, int]
    b: tuple[int, int]
    dest: tuple[int, int]

    @classmethod
    def parse(cls, data: str) -> "Puzzle":
        lines = data.splitlines()
        aline = lines[0]
        bline = lines[1]
        destline = lines[2]

        aparts = aline.split()
        ax = int(aparts[2].removeprefix("X+").removesuffix(","))
        ay = int(aparts[3].removeprefix("Y+"))

        bparts = bline.split()
        bx = int(bparts[2].removeprefix("X+").removesuffix(","))
        by = int(bparts[3].removeprefix("Y+"))

        destparts = destline.split()
        destx = int(destparts[1].removeprefix("X=").removesuffix(","))
        desty = int(destparts[2].removeprefix("Y="))

        return Puzzle((ax, ay), (bx, by), (destx, desty))

    def solve(self) -> Optional[tuple[int, int]]:
        # Simultaneous equations ennit...
        b_presses = (self.a[1] * self.dest[0] - self.a[0] * self.dest[1]) / (
            self.a[1] * self.b[0] - self.a[0] * self.b[1]
        )
        a_presses = (self.dest[0] - self.b[0] * b_presses) / self.a[0]
        if b_presses.is_integer() and a_presses.is_integer():
            return int(a_presses), int(b_presses)
        return None


def p1(data: str) -> int:
    puzzles = [Puzzle.parse(puzz) for puzz in data.strip().split("\n\n")]
    total_score = 0
    for puzzle in puzzles:
        solution = puzzle.solve()
        if solution is None:
            continue
        score = solution[0] * 3 + solution[1]
        total_score += score

    return total_score


ADJUST = 10000000000000


def p2(data: str) -> int:
    puzzles = [Puzzle.parse(puzz) for puzz in data.strip().split("\n\n")]
    modified_puzzles = [
        Puzzle(puzz.a, puzz.b, (puzz.dest[0] + ADJUST, puzz.dest[1] + ADJUST))
        for puzz in puzzles
    ]
    total_score = 0
    for puzzle in modified_puzzles:
        solution = puzzle.solve()
        if solution is None:
            continue
        score = solution[0] * 3 + solution[1]
        total_score += score

    return total_score
