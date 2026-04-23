import bisect
import sys


def max_envelopes(envelopes: list[tuple[int, int]]) -> int:
    envelopes.sort(key=lambda x: (x[0], -x[1]))
    tails: list[int] = []
    for _, h in envelopes:
        i = bisect.bisect_left(tails, h)
        if i == len(tails):
            tails.append(h)
        else:
            tails[i] = h
    return len(tails)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0]); idx = 1; out: list[str] = []
    for tc in range(t):
        n = int(data[idx]); idx += 1
        env = []
        for _ in range(n):
            env.append((int(data[idx]), int(data[idx + 1]))); idx += 2
        if tc: out.append("")
        out.append(str(max_envelopes(env)))
    sys.stdout.write("\n".join(out))

if __name__ == "__main__":
    main()
