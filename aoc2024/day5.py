def _parse(data: str) -> tuple[dict[int, set[int]], list[list[int]]]:
    sections = data.split("\n\n", 1)
    rules_sect = sections[0]
    updates_sect = sections[1]
    rules: dict[int, set[int]] = {}
    for line in rules_sect.splitlines():
        left = int(line.split("|")[0])
        right = int(line.split("|")[-1])
        if left not in rules:
            rules[left] = set()
        rules[left].add(right)
    updates: list[list[int]] = [
        [int(x) for x in line.split(",")] for line in updates_sect.splitlines()
    ]
    return rules, updates


def _sort_by_rules(update: list[int], rules: dict[int, set[int]]) -> list[int]:
    sort_res: list[int] = []
    for entry in update[::-1]:
        before = rules.get(entry, set())
        for i in range(len(sort_res)):
            other = sort_res[i]
            if other in before:
                sort_res = sort_res[:i] + [entry] + sort_res[i:]
                break
        else:
            sort_res = sort_res + [entry]
    return sort_res

def _update_is_valid(update: list[int], rules: dict[int, set[int]]) -> bool:
    return update == _sort_by_rules(update, rules)


def p1(data: str) -> int:
    rules, updates = _parse(data)
    valid_updates = [update for update in updates if _update_is_valid(update, rules)]
    return sum(v[len(v) // 2] for v in valid_updates)


def p2(data: str) -> int:
    rules, updates = _parse(data)
    invalid_updates = [update for update in updates if not _update_is_valid(update, rules)]
    sorted_invalid_updates = [_sort_by_rules(update, rules) for update in invalid_updates]
    return sum(v[len(v) // 2] for v in sorted_invalid_updates)
