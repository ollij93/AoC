from typing import Optional


def _parse(inp: str) -> list[list[int]]:
    return [[int(x) for x in line.split() if x] for line in inp.splitlines()]


def get_bad_index(record: list[str]) -> Optional[int]:
    diff1 = record[1] - record[0]
    if diff1 == 0:
        return 1
    sign1 = diff1 / abs(diff1)
    for i in range(len(record) - 1):
        diff = record[i + 1] - record[i]
        if diff == 0 or abs(diff) > 3:
            return i + 1
        sign = diff / abs(diff)
        if sign != sign1:
            return i + 1


def p1(data: str) -> int:
    vals = _parse(data)
    return len([record for record in vals if get_bad_index(record) is None])


def p2(data: str) -> int:
    vals = _parse(data)
    def is_safe(record: list[int]):
        if get_bad_index(record) is None:
            return True

        for i in range(len(record)):
            cleaned_record = record[0:i] + record[i+1:]
            bi = get_bad_index(cleaned_record)
            if bi is None:
                return True
        return False
    return len([r for r in vals if is_safe(r)])
