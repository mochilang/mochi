import sys


def is_self_crossing(distance: list[int]) -> bool:
    x = distance
    for i in range(3, len(x)):
        if x[i] >= x[i - 2] and x[i - 1] <= x[i - 3]:
            return True
        if i >= 4 and x[i - 1] == x[i - 3] and x[i] + x[i - 4] >= x[i - 2]:
            return True
        if (i >= 5 and x[i - 2] >= x[i - 4]
                and x[i] + x[i - 4] >= x[i - 2]
                and x[i - 1] <= x[i - 3]
                and x[i - 1] + x[i - 5] >= x[i - 3]):
            return True
    return False


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0]); idx = 1; out: list[str] = []
    for tc in range(t):
        n = int(data[idx]); idx += 1
        distance = list(map(int, data[idx:idx + n])); idx += n
        if tc: out.append("")
        out.append("true" if is_self_crossing(distance) else "false")
    sys.stdout.write("\n".join(out))

if __name__ == "__main__":
    main()
