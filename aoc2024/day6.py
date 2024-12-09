import enum
from typing import Callable


class Dir(enum.Enum):
    UP = 1
    RIGHT = 2
    DOWN = 3
    LEFT = 4

    def turn(self) -> "Dir":
        return Dir(1 + (self.value % 4))

    def step(self, pos: tuple[int, int]) -> tuple[int, int]:
        if self == Dir.UP:
            return pos[0] - 1, pos[1]
        elif self == Dir.RIGHT:
            return pos[0], pos[1] + 1
        elif self == Dir.DOWN:
            return pos[0] + 1, pos[1]
        else:
            return pos[0], pos[1] - 1


def _out_of_bounds(coords: tuple[int, int], grid: list[str]) -> bool:
    return (
        coords[0] >= len(grid)
        or coords[0] < 0
        or coords[1] >= len(grid[0])
        or coords[1] < 0
    )


def _process_patrol(
    initial_pos: tuple[int, int],
    grid: list[str],
) -> tuple[set[tuple[int, int]], bool]:
    """
    Process the patrol, returning the points visited and whether it was a loop.
    """
    curr_pos = initial_pos
    curr_dir = Dir.UP
    history = {(curr_pos, curr_dir)}
    while True:
        next_step = curr_dir.step(curr_pos)
        if _out_of_bounds(next_step, grid):
            # Going out of bounds - not a loop
            return {x[0] for x in history}, False
        if (next_step, curr_dir) in history:
            # Have been here before - in a loop
            return {x[0] for x in history}, True

        if grid[next_step[0]][next_step[1]] == "#":
            curr_dir = curr_dir.turn()
        else:
            curr_pos = next_step
            history.add((curr_pos, curr_dir))


def p1(data: str) -> int:
    rows = [line for line in data.splitlines() if line]
    coords = [
        (ri, ci) for ri, row in enumerate(rows) for ci, x in enumerate(row) if x == "^"
    ]
    assert len(coords) == 1
    curr_pos = coords[0]
    history, _ = _process_patrol(curr_pos, rows)
    return len(history)


def p2(data: str) -> int:
    rows = [line for line in data.splitlines() if line]
    coords = [
        (ri, ci) for ri, row in enumerate(rows) for ci, x in enumerate(row) if x == "^"
    ]
    assert len(coords) == 1
    curr_pos = coords[0]
    points_to_test, _ = _process_patrol(curr_pos, rows)
    num_loops = 0
    for point in points_to_test:
        new_grid = rows.copy()
        new_grid[point[0]] = (
            new_grid[point[0]][: point[1]] + "#" + new_grid[point[0]][point[1] + 1 :]
        )
        _, loop = _process_patrol(curr_pos, new_grid)
        if loop:
            num_loops += 1
    return num_loops
