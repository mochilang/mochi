import sys


def min_area(image: list[str], x: int, y: int) -> int:
    top = len(image)
    bottom = -1
    left = len(image[0])
    right = -1
    for i, row in enumerate(image):
        for j, ch in enumerate(row):
            if ch == "1":
                top = min(top, i)
                bottom = max(bottom, i)
                left = min(left, j)
                right = max(right, j)
    return (bottom - top + 1) * (right - left + 1)


def main() -> None:
    lines = [line.strip() for line in sys.stdin if line.strip()]
    if not lines:
        return
    t = int(lines[0])
    idx = 1
    out: list[str] = []
    for tc in range(t):
        r, c = map(int, lines[idx].split())
        idx += 1
        image = lines[idx:idx + r]
        idx += r
        x, y = map(int, lines[idx].split())
        idx += 1
        if tc:
            out.append("")
        out.append(str(min_area(image, x, y)))
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
