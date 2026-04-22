import sys


def solve(m: int, n: int, positions: list[tuple[int, int]]) -> list[int]:
    parent = {}
    rank = {}
    count = 0
    ans = []

    def find(x: int) -> int:
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def union(a: int, b: int) -> bool:
        ra = find(a)
        rb = find(b)
        if ra == rb:
            return False
        if rank[ra] < rank[rb]:
            ra, rb = rb, ra
        parent[rb] = ra
        if rank[ra] == rank[rb]:
            rank[ra] += 1
        return True

    for r, c in positions:
        idx = r * n + c
        if idx in parent:
            ans.append(count)
            continue
        parent[idx] = idx
        rank[idx] = 0
        count += 1
        for dr, dc in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            nr, nc = r + dr, c + dc
            if 0 <= nr < m and 0 <= nc < n:
                nei = nr * n + nc
                if nei in parent and union(idx, nei):
                    count -= 1
        ans.append(count)
    return ans


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for tc in range(t):
        m = int(data[idx])
        n = int(data[idx + 1])
        k = int(data[idx + 2])
        idx += 3
        positions = []
        for _ in range(k):
            positions.append((int(data[idx]), int(data[idx + 1])))
            idx += 2
        if tc:
            out.append("")
        out.append(str(solve(m, n, positions)).replace(" ", ""))
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
