def _parse(inp: str) -> list[tuple[int, int]]:
    return [
        (int(x.split()[0].strip()), int(x.rsplit()[-1].strip()))
        for x in inp.splitlines()
    ]


def p1(data) -> int:
    vals = _parse(data)
    a = sorted(v[0] for v in vals)
    b = sorted(v[1] for v in vals)
    total = 0
    for lft, rgt in zip(a, b):
        diff = abs(lft - rgt)
        total += diff

    return total

def p2(data) -> int:
    vals = _parse(data)
    counts = {}
    for _, x in vals:
        if x not in counts:
            counts[x] = 0
        counts[x] += 1

    total = 0
    for x, _ in vals:
        sim = x * counts.get(x, 0)
        total += sim
    return total
