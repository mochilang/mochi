import sys


def solve(edges: list[list[int]]) -> list[int]:
    n = len(edges)
    cand_a: list[int] | None = None
    cand_b: list[int] | None = None
    parent_of = [0] * (n + 1)
    for u, v in edges:
        if parent_of[v] == 0:
            parent_of[v] = u
        else:
            cand_a = [parent_of[v], v]
            cand_b = [u, v]
            break

    parent = list(range(n + 1))

    def find(x: int) -> int:
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def union(a: int, b: int) -> bool:
        ra, rb = find(a), find(b)
        if ra == rb:
            return False
        parent[rb] = ra
        return True

    for u, v in edges:
        if cand_b is not None and [u, v] == cand_b:
            continue
        if not union(u, v):
            return cand_a if cand_a is not None else [u, v]
    assert cand_b is not None
    return cand_b


def fmt(edge: list[int]) -> str:
    return "[" + ",".join(map(str, edge)) + "]"


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx])
        idx += 1
        edges = []
        for _ in range(n):
            u = int(data[idx])
            v = int(data[idx + 1])
            idx += 2
            edges.append([u, v])
        out.append(fmt(solve(edges)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
