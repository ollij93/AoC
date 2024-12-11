from collections import defaultdict

def _run_sim(data: str, num_iters: int) -> int:
    inp_stones = [int(n) for n in data.strip().split()]
    curr_stones: dict[int, int] = {}
    for stone in inp_stones:
        if stone not in curr_stones:
            curr_stones[stone] = 0
        curr_stones[stone] += 1

    def step(stones: dict[int, int]) -> dict[int, int]:
        new_stones: dict[int, int] = defaultdict(lambda: 0)
        for stone, count in stones.items():
            if stone == 0:
                new_stones[1] += count
            elif len(str(stone)) % 2 == 0:
                half_digits = len(str(stone))//2
                front_num = int(str(stone)[:half_digits])
                back_num = int(str(stone)[half_digits:])
                new_stones[front_num] += count
                new_stones[back_num] += count
            else:
                new_stones[stone * 2024] += count
        return dict(new_stones)
    for _ in range(num_iters):
        curr_stones = step(curr_stones)
    total = sum(curr_stones.values())
    return total

def p1(data: str) -> int:
    return _run_sim(data, 25)

def p2(data: str) -> int:
    return _run_sim(data, 75)
