import sys


def solve(points: list[tuple[int, int]]) -> list[tuple[int, int]]:
    pts = sorted(set(points))
    if len(pts) <= 1:
        return pts

    def cross(o: tuple[int, int], a: tuple[int, int], b: tuple[int, int]) -> int:
        return (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])

    lower: list[tuple[int, int]] = []
    for p in pts:
        while len(lower) >= 2 and cross(lower[-2], lower[-1], p) < 0:
            lower.pop()
        lower.append(p)

    upper: list[tuple[int, int]] = []
    for p in reversed(pts):
        while len(upper) >= 2 and cross(upper[-2], upper[-1], p) < 0:
            upper.pop()
        upper.append(p)

    return sorted(set(lower + upper))


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    blocks: list[str] = []
    for tc in range(t):
        n = int(data[idx]); idx += 1
        pts = []
        for _ in range(n):
            x = int(data[idx]); y = int(data[idx + 1]); idx += 2
            pts.append((x, y))
        hull = solve(pts)
        if tc:
            blocks.append("")
        blocks.append(str(len(hull)))
        blocks.extend(f"{x} {y}" for x, y in hull)
    sys.stdout.write("\n".join(blocks))


if __name__ == "__main__":
    main()
