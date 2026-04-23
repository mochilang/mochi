import sys


def longest_increasing_path(matrix: list[list[int]]) -> int:
    rows, cols = len(matrix), len(matrix[0])
    memo = [[0] * cols for _ in range(rows)]

    def dfs(r: int, c: int) -> int:
        if memo[r][c]:
            return memo[r][c]
        best = 1
        for dr, dc in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols and matrix[nr][nc] > matrix[r][c]:
                best = max(best, 1 + dfs(nr, nc))
        memo[r][c] = best
        return best

    return max(dfs(r, c) for r in range(rows) for c in range(cols))


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for tc in range(t):
        rows, cols = int(data[idx]), int(data[idx + 1])
        idx += 2
        matrix = []
        for _ in range(rows):
            matrix.append(list(map(int, data[idx:idx + cols])))
            idx += cols
        if tc:
            out.append("")
        out.append(str(longest_increasing_path(matrix)))
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
