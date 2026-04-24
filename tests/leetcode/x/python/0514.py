import sys


def solve(ring: str, key: str) -> int:
    n = len(ring)
    positions: dict[str, list[int]] = {}
    for i, ch in enumerate(ring):
        positions.setdefault(ch, []).append(i)
    dp = {0: 0}
    for ch in key:
        nxt: dict[int, int] = {}
        for j in positions[ch]:
            best = 10**18
            for i, cost in dp.items():
                diff = abs(i - j)
                step = min(diff, n - diff)
                cand = cost + step
                if cand < best:
                    best = cand
            nxt[j] = best
        dp = nxt
    return min(dp.values()) + len(key)


def main() -> None:
    lines = [line.rstrip("\n") for line in sys.stdin.readlines()]
    if not lines:
        return
    t = int(lines[0].strip())
    idx = 1
    out: list[str] = []
    for _ in range(t):
        ring = lines[idx]
        key = lines[idx + 1]
        idx += 2
        out.append(str(solve(ring, key)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
