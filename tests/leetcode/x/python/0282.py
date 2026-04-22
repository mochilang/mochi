import sys


def solve(num, target):
    ans = []

    def dfs(i, expr, value, last):
        if i == len(num):
            if value == target:
                ans.append(expr)
            return
        for j in range(i, len(num)):
            if j > i and num[i] == "0":
                break
            s = num[i : j + 1]
            n = int(s)
            if i == 0:
                dfs(j + 1, s, n, n)
            else:
                dfs(j + 1, expr + "+" + s, value + n, n)
                dfs(j + 1, expr + "-" + s, value - n, -n)
                dfs(j + 1, expr + "*" + s, value - last + last * n, last * n)

    dfs(0, "", 0, 0)
    ans.sort()
    return ans


lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0].strip())
    out = []
    idx = 1
    for _ in range(t):
        num = lines[idx].strip()
        target = int(lines[idx + 1].strip())
        idx += 2
        ans = solve(num, target)
        out.append("\n".join([str(len(ans))] + ans))
    sys.stdout.write("\n\n".join(out))
