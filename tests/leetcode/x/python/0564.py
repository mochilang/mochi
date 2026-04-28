import sys


def solve(n: str) -> str:
    m = len(n)
    x = int(n)
    candidates = {10 ** (m - 1) - 1, 10 ** m + 1}
    prefix = int(n[: (m + 1) // 2])
    for delta in (-1, 0, 1):
        p = str(prefix + delta)
        if m % 2 == 0:
            pal = p + p[::-1]
        else:
            pal = p + p[-2::-1]
        candidates.add(int(pal))
    candidates.discard(x)
    best = None
    for cand in candidates:
        if cand < 0:
            continue
        if best is None:
            best = cand
            continue
        if abs(cand - x) < abs(best - x) or (abs(cand - x) == abs(best - x) and cand < best):
            best = cand
    return str(best)


def main() -> None:
    lines = [line.rstrip("\n") for line in sys.stdin]
    if not lines:
        return
    t = int(lines[0].strip())
    out = [solve(lines[i + 1].strip()) for i in range(t)]
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
