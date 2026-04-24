import sys


def solve(machines: list[int]) -> int:
    total = sum(machines)
    n = len(machines)
    if total % n != 0:
        return -1
    target = total // n
    flow = 0
    ans = 0
    for x in machines:
        diff = x - target
        flow += diff
        ans = max(ans, abs(flow), diff)
    return ans


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        machines = list(map(int, data[idx:idx + n]))
        idx += n
        out.append(str(solve(machines)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
