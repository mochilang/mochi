import sys


def intervals(seen: set[int]) -> list[list[int]]:
    vals = sorted(seen)
    out: list[list[int]] = []
    for v in vals:
        if not out or v > out[-1][1] + 1:
            out.append([v, v])
        else:
            out[-1][1] = v
    return out


def fmt_intervals(ranges: list[list[int]]) -> str:
    return "[" + ",".join(f"[{a},{b}]" for a, b in ranges) + "]"


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0]); idx = 1; cases: list[str] = []
    for _ in range(t):
        ops = int(data[idx]); idx += 1
        seen: set[int] = set()
        snaps: list[str] = []
        for _ in range(ops):
            op = data[idx]; idx += 1
            if op == "A":
                seen.add(int(data[idx])); idx += 1
            else:
                snaps.append(fmt_intervals(intervals(seen)))
        cases.append("[" + ",".join(snaps) + "]")
    sys.stdout.write("\n\n".join(cases))

if __name__ == "__main__":
    main()
