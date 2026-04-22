import sys

def solve(vals, ok):
    best = -10**18
    def dfs(i):
        nonlocal best
        if i >= len(vals) or not ok[i]:
            return 0
        left = max(0, dfs(2 * i + 1))
        right = max(0, dfs(2 * i + 2))
        total = vals[i] + left + right
        if total > best:
            best = total
        return vals[i] + max(left, right)
    dfs(0)
    return best

lines = sys.stdin.read().splitlines()
if lines:
    tc = int(lines[0])
    idx = 1
    out = []
    for _ in range(tc):
        n = int(lines[idx]); idx += 1
        vals = [0] * n
        ok = [False] * n
        for i in range(n):
            tok = lines[idx].strip(); idx += 1
            if tok != 'null':
                ok[i] = True
                vals[i] = int(tok)
        out.append(str(solve(vals, ok)))
    sys.stdout.write('\n'.join(out))
