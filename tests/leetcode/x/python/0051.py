import sys

def solve_n_queens(n):
    cols = [False] * n
    d1 = [False] * (2 * n)
    d2 = [False] * (2 * n)
    board = [['.'] * n for _ in range(n)]
    res = []
    def dfs(r):
        if r == n:
            res.append([''.join(row) for row in board])
            return
        for c in range(n):
            a = r + c
            b = r - c + n - 1
            if cols[c] or d1[a] or d2[b]:
                continue
            cols[c] = d1[a] = d2[b] = True
            board[r][c] = 'Q'
            dfs(r + 1)
            board[r][c] = '.'
            cols[c] = d1[a] = d2[b] = False
    dfs(0)
    return res

def main():
    lines = sys.stdin.read().splitlines()
    if not lines: return
    idx = 0
    t = int(lines[idx].strip()); idx += 1
    out = []
    for tc in range(t):
        n = int(lines[idx].strip()); idx += 1
        sols = solve_n_queens(n)
        out.append(str(len(sols)))
        for si, sol in enumerate(sols):
            out.extend(sol)
            if si + 1 < len(sols): out.append('-')
        if tc + 1 < t: out.append('=')
    sys.stdout.write('\n'.join(out))

if __name__ == '__main__': main()
