import sys


def is_rectangle_cover(rects: list[tuple[int, int, int, int]]) -> bool:
    min_x = min_y = 10**18
    max_x = max_y = -10**18
    area = 0
    corners: set[tuple[int, int]] = set()
    for x1, y1, x2, y2 in rects:
        min_x = min(min_x, x1); min_y = min(min_y, y1)
        max_x = max(max_x, x2); max_y = max(max_y, y2)
        area += (x2 - x1) * (y2 - y1)
        for p in ((x1, y1), (x1, y2), (x2, y1), (x2, y2)):
            if p in corners:
                corners.remove(p)
            else:
                corners.add(p)
    expected = (max_x - min_x) * (max_y - min_y)
    outer = {(min_x, min_y), (min_x, max_y), (max_x, min_y), (max_x, max_y)}
    return area == expected and corners == outer


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0]); idx = 1; out: list[str] = []
    for tc in range(t):
        n = int(data[idx]); idx += 1
        rects = []
        for _ in range(n):
            rects.append(tuple(map(int, data[idx:idx + 4]))); idx += 4
        if tc: out.append("")
        out.append("true" if is_rectangle_cover(rects) else "false")
    sys.stdout.write("\n".join(out))
if __name__ == "__main__": main()
