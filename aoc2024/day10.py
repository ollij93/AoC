from typing import Iterator

def _parse(data: str) -> dict[tuple[int, int], set[tuple[int, int]]]:
    links: dict[tuple[int, int], set[tuple[int, int]]] = {}
    rows = data.strip().splitlines()
    for ri, row in enumerate(rows):
        for ci, numstr in enumerate(row):
            num = int(numstr)
            links[(ri, ci)] = set()
            compares = [
                (ri + 1, ci),
                (ri - 1, ci),
                (ri, ci + 1),
                (ri, ci - 1),
            ]
            for point in compares:
                if (
                    point[0] >= len(rows)
                    or point[0] < 0
                    or point[1] >= len(row)
                    or point[1] < 0
                ):
                    continue
                val = int(rows[point[0]][point[1]])
                if val == num + 1:
                    links[(ri, ci)].add(point)
    return links


def _get_visited(
    point: tuple[int, int], links: dict[tuple[int, int], set[tuple[int, int]]]
) -> Iterator[tuple[int, int]]:
    yield point
    for other in links[point]:
        yield from _get_visited(other, links)


def p1(data: str) -> int:
    links = _parse(data)
    rows = data.strip().splitlines()
    score = 0
    for ri, row in enumerate(rows):
        for ci, numstr in enumerate(row):
            num = int(numstr)
            if num == 0:
                visited = set(_get_visited((ri, ci), links))
                for point in visited:
                    val = int(rows[point[0]][point[1]])
                    if val == 9:
                        score += 1
    return score

def _get_paths(point: tuple[int, int], links: dict[tuple[int, int], set[tuple[int, int]]]) -> Iterator[list[tuple[int, int]]]:
    path = [point]
    yield path
    for other in links[point]:
        subpaths = _get_paths(other, links)
        yield from (
            path + subpath for subpath in subpaths
        )


def p2(data: str) -> int:
    links = _parse(data)
    rows = data.strip().splitlines()
    score = 0
    for ri, row in enumerate(rows):
        for ci, numstr in enumerate(row):
            num = int(numstr)
            if num == 0:
                paths = _get_paths((ri, ci), links)
                for path in paths:
                    endpoint = path[-1]
                    if int(rows[endpoint[0]][endpoint[1]]) == 9:
                        score += 1
    return score
