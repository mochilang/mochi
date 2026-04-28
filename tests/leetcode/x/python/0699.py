import sys


def solve(positions: list[tuple[int, int]]) -> list[int]:
    heights = [0] * len(positions)
    answer: list[int] = []
    best = 0
    for i, (left, size) in enumerate(positions):
        right = left + size
        base = 0
        for j in range(i):
            other_left, other_size = positions[j]
            other_right = other_left + other_size
            if left < other_right and other_left < right:
                base = max(base, heights[j])
        heights[i] = base + size
        best = max(best, heights[i])
        answer.append(best)
    return answer


def fmt(values: list[int]) -> str:
    return "[" + ",".join(map(str, values)) + "]"


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx])
        idx += 1
        positions: list[tuple[int, int]] = []
        for _ in range(n):
            left = int(data[idx])
            size = int(data[idx + 1])
            idx += 2
            positions.append((left, size))
        out.append(fmt(solve(positions)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
