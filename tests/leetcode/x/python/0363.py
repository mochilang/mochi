import bisect
import sys


def max_sum_submatrix(matrix: list[list[int]], k: int) -> int:
    rows, cols = len(matrix), len(matrix[0])
    best = -10**18
    for top in range(rows):
        sums = [0] * cols
        for bottom in range(top, rows):
            for c in range(cols):
                sums[c] += matrix[bottom][c]
            prefixes = [0]
            cur = 0
            for x in sums:
                cur += x
                i = bisect.bisect_left(prefixes, cur - k)
                if i < len(prefixes):
                    best = max(best, cur - prefixes[i])
                bisect.insort(prefixes, cur)
    return int(best)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0]); idx = 1; out: list[str] = []
    for tc in range(t):
        r, c = int(data[idx]), int(data[idx + 1]); idx += 2
        matrix = []
        for _ in range(r):
            matrix.append(list(map(int, data[idx:idx + c]))); idx += c
        k = int(data[idx]); idx += 1
        if tc: out.append("")
        out.append(str(max_sum_submatrix(matrix, k)))
    sys.stdout.write("\n".join(out))
if __name__ == "__main__": main()
