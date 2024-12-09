def _parse(data: str) -> dict[str, set[tuple[int, int]]]:
    grid = data.splitlines()
    res: dict[str, set[tuple[int, int]]] = {}
    for ri, row in enumerate(grid):
        for ci, char in enumerate(row):
            if char == ".":
                continue
            if char not in res:
                res[char] = set()
            res[char].add((ri, ci))
    return res


def p1(data: str) -> int:
    grid = data.splitlines()
    antenna = _parse(data)
    antinodes: set[tuple[int, int]] = set()
    for _, points in antenna.items():
        for point_a in points:
            for point_b in points - {point_a}:
                diff = (point_a[0] - point_b[0], point_a[1] - point_b[1])
                antinode = (diff[0] + point_a[0], diff[1] + point_a[1])
                antinodes.add(antinode)
    in_bounds = {
        antinode
        for antinode in antinodes
        if antinode[0] >= 0
        and antinode[0] < len(grid)
        and antinode[1] >= 0
        and antinode[1] < len(grid[0])
    }
    return len(in_bounds)


def p2(data: str) -> int:
    grid = data.splitlines()
    antenna = _parse(data)
    antinodes: set[tuple[int, int]] = set()
    for _, points in antenna.items():
        for point_a in points:
            for point_b in points - {point_a}:
                diff = (point_a[0] - point_b[0], point_a[1] - point_b[1])
                antinode = point_a
                while (
                    antinode[0] >= 0
                    and antinode[0] < len(grid)
                    and antinode[1] >= 0
                    and antinode[1] < len(grid[0])
                ):
                    antinodes.add(antinode)
                    antinode = (diff[0] + antinode[0], diff[1] + antinode[1])
    return len(antinodes)
