import sys

def poor_pigs(buckets: int, minutes_to_die: int, minutes_to_test: int) -> int:
    states = minutes_to_test // minutes_to_die + 1
    pigs = 0
    capacity = 1
    while capacity < buckets:
        pigs += 1
        capacity *= states
    return pigs

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    ans: list[str] = []
    for _ in range(t):
        b = int(data[idx]); d = int(data[idx + 1]); m = int(data[idx + 2]); idx += 3
        ans.append(str(poor_pigs(b, d, m)))
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
