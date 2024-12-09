def _parse(data: str) -> list[tuple[int, int]]:
    data = data.strip()
    ret: list[tuple[int, int]] = []
    nid: int = 0
    while data:
        ret.append((nid, int(data[0])))
        data = data[1:]
        if data:
            ret.append((-1, int(data[0])))
            data = data[1:]
        nid += 1
    return ret


def pprint(segments: list[tuple[int, int]]) -> None:
    for segment in segments:
        for _ in range(segment[1]):
            print(segment[0] if segment[0] != -1 else ".", end="")
    print()


def p1(data: str) -> int:
    inp = _parse(data)

    def proc(
        inp: list[tuple[int, int]]
    ) -> tuple[list[tuple[int, int]], list[tuple[int, int]]]:
        pre: list[tuple[int, int]] = []
        while inp[0][0] != -1:
            pre.append(inp[0])
            inp = inp[1:]
        to_fill = inp[0]
        post = inp[1:]
        back = post[-1]
        if to_fill[1] < back[1]:
            return (
                pre + [(back[0], to_fill[1])],
                post[:-1] + [(back[0], back[1] - to_fill[1])],
            )
        elif to_fill[1] == back[1]:
            return pre + [(back[0], back[1])], post[:-2]
        else:
            return pre + [(back[0], back[1])], [(-1, to_fill[1] - back[1])] + post[:-2]

    res = []
    remainder = inp
    x = 0
    while any(i[0] == -1 for i in remainder):
        # print(x, len([i for i in remainder if i[0] == -1]))
        step_res, remainder = proc(remainder)
        res.extend(step_res)
        x += 1
    res += remainder
    # pprint(res)

    c = 0
    result = 0
    for segment in res:
        val = segment[0]
        for idx in range(c, c + segment[1]):
            result += val * idx
        c += segment[1]

    return result


def p2(data: str) -> int:
    inp = _parse(data)
    free_segs = list(range(1, len(inp), 2))
    segs_to_proc = list(range(0, len(inp), 2))[::-1]

    step = inp

    while segs_to_proc:
        segi = segs_to_proc.pop(0)
        seg = step[segi]
        for free_segi in [fsi for fsi in free_segs if fsi < segi]:
            free_seg = step[free_segi]
            if free_seg[1] > seg[1]:
                step = (
                    step[:free_segi]
                    + [seg]
                    + [(-1, free_seg[1] - seg[1])]
                    + step[free_segi + 1 : segi]
                    + [(-1, seg[1])]
                    + step[segi + 1 :]
                )
                free_segs = [
                    (fsi + 1) if fsi >= free_segi else fsi for fsi in free_segs
                ]
                segs_to_proc = [
                    (si + 1) if si >= free_segi else si for si in segs_to_proc
                ]
                break
            elif free_seg[1] == seg[1]:
                step = (
                    step[:free_segi]
                    + [seg]
                    + step[free_segi + 1 :segi]
                    + [(-1, seg[1])]
                    + step[segi + 1 :]
                )
                free_segs.remove(free_segi)
                break
    res = step

    c = 0
    result = 0
    for segment in res:
        val = segment[0]
        if val != -1:
            for idx in range(c, c + segment[1]):
                result += val * idx
        c += segment[1]

    return result
