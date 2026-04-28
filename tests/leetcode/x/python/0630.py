import heapq
import sys


def solve(courses: list[tuple[int, int]]) -> int:
    courses.sort(key=lambda x: x[1])
    total = 0
    heap: list[int] = []
    for duration, last_day in courses:
        total += duration
        heapq.heappush(heap, -duration)
        if total > last_day:
            total += heapq.heappop(heap)
    return len(heap)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        courses = []
        for _ in range(n):
            duration = int(data[idx]); last_day = int(data[idx + 1]); idx += 2
            courses.append((duration, last_day))
        out.append(str(solve(courses)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
