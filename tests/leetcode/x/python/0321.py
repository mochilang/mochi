import sys


def pick(nums: list[int], k: int) -> list[int]:
    drop = len(nums) - k
    stack: list[int] = []
    for x in nums:
        while drop and stack and stack[-1] < x:
            stack.pop()
            drop -= 1
        stack.append(x)
    return stack[:k]


def merge(a: list[int], b: list[int]) -> list[int]:
    out: list[int] = []
    i = j = 0
    while i < len(a) or j < len(b):
        if a[i:] > b[j:]:
            out.append(a[i])
            i += 1
        else:
            out.append(b[j])
            j += 1
    return out


def max_number(nums1: list[int], nums2: list[int], k: int) -> list[int]:
    best: list[int] = []
    start = max(0, k - len(nums2))
    end = min(k, len(nums1))
    for take1 in range(start, end + 1):
        cand = merge(pick(nums1, take1), pick(nums2, k - take1))
        if cand > best:
            best = cand
    return best


def fmt(a: list[int]) -> str:
    return "[" + ",".join(map(str, a)) + "]"


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for tc in range(t):
        n1 = int(data[idx])
        idx += 1
        nums1 = list(map(int, data[idx:idx + n1]))
        idx += n1
        n2 = int(data[idx])
        idx += 1
        nums2 = list(map(int, data[idx:idx + n2]))
        idx += n2
        k = int(data[idx])
        idx += 1
        if tc:
            out.append("")
        out.append(fmt(max_number(nums1, nums2, k)))
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
