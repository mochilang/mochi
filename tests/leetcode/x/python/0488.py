import sys
from collections import Counter


def shrink_board(s: str) -> str:
    changed = True
    while changed:
        changed = False
        out: list[str] = []
        i = 0
        while i < len(s):
            j = i
            while j < len(s) and s[j] == s[i]:
                j += 1
            if j - i >= 3:
                changed = True
            else:
                out.append(s[i:j])
            i = j
        s = "".join(out)
    return s


def solve(board: str, hand: str) -> int:
    start = Counter(hand)
    memo: dict[tuple[str, tuple[int, int, int, int, int]], int] = {}
    colors = "RYBGW"

    def dfs(board: str, cnt: dict[str, int]) -> int:
        board = shrink_board(board)
        if not board:
            return 0
        state = (board, tuple(cnt[c] for c in colors))
        if state in memo:
            return memo[state]
        best = 10**9
        i = 0
        while i < len(board):
            j = i
            while j < len(board) and board[j] == board[i]:
                j += 1
            need = max(0, 3 - (j - i))
            color = board[i]
            if cnt[color] >= need:
                cnt[color] -= need
                sub = dfs(board[:i] + board[j:], cnt)
                if sub != 10**9:
                    best = min(best, need + sub)
                cnt[color] += need
            i = j
        memo[state] = best
        return best

    ans = dfs(board, start)
    return -1 if ans == 10**9 else ans


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    out: list[str] = []
    for _ in range(t):
        board = data[idx]; hand = data[idx + 1]; idx += 2
        out.append(str(solve(board, hand)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
