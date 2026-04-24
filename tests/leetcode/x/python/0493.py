import sys


def sort_count(a: list[int]) -> tuple[list[int], int]:
    if len(a) <= 1:
        return a[:], 0
    m = len(a) // 2
    left, c1 = sort_count(a[:m])
    right, c2 = sort_count(a[m:])
    cnt = c1 + c2
    j = 0
    for x in left:
        while j < len(right) and x > 2 * right[j]:
            j += 1
        cnt += j
    merged: list[int] = []
    i = j = 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            merged.append(left[i])
            i += 1
        else:
            merged.append(right[j])
            j += 1
    merged.extend(left[i:])
    merged.extend(right[j:])
    return merged, cnt


def solve(nums: list[int]) -> int:
    return sort_count(nums)[1]


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        nums = list(map(int, data[idx:idx + n]))
        idx += n
        out.append(str(solve(nums)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
