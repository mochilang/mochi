import sys


def count_range_sum(nums: list[int], lower: int, upper: int) -> int:
    prefixes = [0]
    for x in nums:
        prefixes.append(prefixes[-1] + x)

    def sort(lo: int, hi: int) -> int:
        if hi - lo <= 1:
            return 0
        mid = (lo + hi) // 2
        ans = sort(lo, mid) + sort(mid, hi)
        left = right = lo
        for r in range(mid, hi):
            while left < mid and prefixes[left] < prefixes[r] - upper:
                left += 1
            while right < mid and prefixes[right] <= prefixes[r] - lower:
                right += 1
            ans += right - left
        prefixes[lo:hi] = sorted(prefixes[lo:hi])
        return ans

    return sort(0, len(prefixes))


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for tc in range(t):
        n = int(data[idx])
        idx += 1
        nums = list(map(int, data[idx:idx + n]))
        idx += n
        lower = int(data[idx])
        upper = int(data[idx + 1])
        idx += 2
        if tc:
            out.append("")
        out.append(str(count_range_sum(nums, lower, upper)))
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
