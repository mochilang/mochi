import sys

def count_prefix(n: int, prefix: int) -> int:
    steps = 0
    first = prefix
    next_prefix = prefix + 1
    while first <= n:
        steps += min(n + 1, next_prefix) - first
        first *= 10
        next_prefix *= 10
    return steps

def find_kth_number(n: int, k: int) -> int:
    cur = 1
    k -= 1
    while k:
        steps = count_prefix(n, cur)
        if steps <= k:
            cur += 1
            k -= steps
        else:
            cur *= 10
            k -= 1
    return cur

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    ans: list[str] = []
    for _ in range(t):
        n = int(data[idx]); k = int(data[idx + 1]); idx += 2
        ans.append(str(find_kth_number(n, k)))
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
