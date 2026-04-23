import sys

def can_cross(stones: list[int]) -> bool:
    positions = {x: i for i, x in enumerate(stones)}
    jumps = [set() for _ in stones]
    jumps[0].add(0)
    last = len(stones) - 1
    for i, stone in enumerate(stones):
        for k in list(jumps[i]):
            for step in (k - 1, k, k + 1):
                if step <= 0:
                    continue
                j = positions.get(stone + step)
                if j is None:
                    continue
                if j == last:
                    return True
                jumps[j].add(step)
    return last == 0

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    ans: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        stones = [int(x) for x in data[idx:idx+n]]; idx += n
        ans.append('true' if can_cross(stones) else 'false')
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
