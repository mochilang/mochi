import sys


def count_smaller(nums: list[int]) -> list[int]:
    n = len(nums)
    counts = [0] * n
    idx = list(range(n))
    tmp = [0] * n

    def sort(lo: int, hi: int) -> None:
        if hi - lo <= 1:
            return
        mid = (lo + hi) // 2
        sort(lo, mid)
        sort(mid, hi)
        i, j, k = lo, mid, lo
        moved = 0
        while i < mid and j < hi:
            if nums[idx[j]] < nums[idx[i]]:
                tmp[k] = idx[j]
                j += 1
                moved += 1
            else:
                counts[idx[i]] += moved
                tmp[k] = idx[i]
                i += 1
            k += 1
        while i < mid:
            counts[idx[i]] += moved
            tmp[k] = idx[i]
            i += 1
            k += 1
        while j < hi:
            tmp[k] = idx[j]
            j += 1
            k += 1
        idx[lo:hi] = tmp[lo:hi]

    sort(0, n)
    return counts


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    pos = 1
    out: list[str] = []
    for tc in range(t):
        n = int(data[pos])
        pos += 1
        nums = list(map(int, data[pos:pos + n]))
        pos += n
        if tc:
            out.append("")
        out.append(str(count_smaller(nums)).replace(" ", ""))
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
