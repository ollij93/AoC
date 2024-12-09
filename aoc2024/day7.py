from dataclasses import dataclass
from typing import Iterator


@dataclass
class Sum:
    result: int
    operands: list[int]

    def possible(self, allow_concat: bool = False) -> bool:
        if len(self.operands) == 1:
            return self.operands[0] == self.result
        subadd = Sum(self.result - self.operands[-1], self.operands[:-1])
        if subadd.possible(allow_concat=allow_concat):
            return True
        if self.result % self.operands[-1] == 0:
            submul = Sum(self.result // self.operands[-1], self.operands[:-1])
            if submul.possible(allow_concat=allow_concat):
                return True
        if not allow_concat:
            return False
        rstr = str(self.operands[-1])
        if not str(self.result).endswith(rstr):
            return False
        res_str = str(self.result)[: -len(rstr)]
        if not res_str:
            return False
        concat_sum = Sum(int(res_str), self.operands[:-1])
        return concat_sum.possible(allow_concat=allow_concat)


def _parse(data: str) -> Iterator[Sum]:
    for line in data.strip().splitlines():
        parts = line.split()
        result = int(parts[0].strip(":"))
        operands = [int(part) for part in parts[1:]]
        yield Sum(result, operands)


def p1(data: str) -> int:
    sums = _parse(data)
    return sum([sm.result for sm in sums if sm.possible()])


def p2(data: str) -> int:
    sums = _parse(data)
    poss = [sm for sm in sums if sm.possible(allow_concat=True)]
    return sum([sm.result for sm in poss])
