import sys
from collections import deque


def solve(words):
    chars = set("".join(words))
    adj = {c: set() for c in chars}
    indeg = {c: 0 for c in chars}
    for a, b in zip(words, words[1:]):
        m = min(len(a), len(b))
        if a[:m] == b[:m] and len(a) > len(b):
            return ""
        for x, y in zip(a, b):
            if x != y:
                if y not in adj[x]:
                    adj[x].add(y)
                    indeg[y] += 1
                break
    q = deque(sorted([c for c in chars if indeg[c] == 0]))
    out = []
    while q:
        c = q.popleft()
        out.append(c)
        for nei in sorted(adj[c]):
            indeg[nei] -= 1
            if indeg[nei] == 0:
                q.append(nei)
    return "".join(out) if len(out) == len(chars) else ""


lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0].strip())
    idx = 1
    out = []
    for _ in range(t):
        n = int(lines[idx].strip())
        idx += 1
        words = [lines[idx + i].strip() for i in range(n)]
        idx += n
        out.append(solve(words))
    sys.stdout.write("\n".join(out))
