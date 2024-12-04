import logging

_logger = logging.getLogger()


def _get_diagonals(grid: list[str]) -> list[str]:
    diags: list[str] = []
    for i in range(len(grid[0]) * 2):
        diag = ""
        for col in range(i + 1):
            row = i - col
            if row < len(grid) and col < len(grid[row]):
                diag += grid[row][col]
            else:
                diag += " "
        if diag:
            diags.append(diag)
    return diags


def p1(data: str) -> int:
    rows = [line for line in data.splitlines() if line]
    num_rows = len(rows)
    num_cols = len(rows[0])
    cols = [
        "".join(rows[row][col] for row in range(num_rows)) for col in range(num_cols)
    ]

    diagsA: list[str] = _get_diagonals(rows)
    diagsB: list[str] = _get_diagonals([r[::-1] for r in rows])

    return sum(
        (line.count("XMAS") + line.count("SAMX"))
        for line in (rows + cols + diagsA + diagsB)
    )


def _get_indexes(string: str, substr: str) -> set[int]:
    if len(string) < len(substr):
        return set()
    ret: set[int] = set()
    for i in range(len(string) - len(substr) + 1):
        idx = string[i:].find(substr)
        if idx != -1:
            ret.add(i + idx)
    return ret - {-1}


def p2(data: str) -> int:
    rows = [line for line in data.splitlines() if line]
    diagsA: list[str] = _get_diagonals(rows)
    diagsB: list[str] = _get_diagonals([r[::-1] for r in rows])

    pointsA: set[tuple[int, int]] = set()
    for n, diag in enumerate(diagsA):
        idxs = _get_indexes(diag, "MAS") | _get_indexes(diag, "SAM")
        # Add the +1 for the A rather than the start of MAS
        idxs = {idx + 1 for idx in idxs}
        # Convert back to the original XY coords
        points = {(n - idx, idx) for idx in idxs}
        pointsA.update(points)
    pointsB: set[tuple[int, int]] = set()
    for n, diag in enumerate(diagsB):
        idxs = _get_indexes(diag, "MAS") | _get_indexes(diag, "SAM")
        # Add the +1 for the A rather than the start of MAS
        idxs = {idx + 1 for idx in idxs}
        # Convert back to the original XY coords
        points = {(n - idx, len(rows[0]) - idx - 1) for idx in idxs}
        pointsB.update(points)
    for ri, row in enumerate(rows):
        line = ""
        for ci, c in enumerate(row):
            line += c if (ri, ci) in pointsA & pointsB else "."
        _logger.debug(line)
    return len(pointsA & pointsB)
