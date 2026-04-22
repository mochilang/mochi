import sys


class Node:
    __slots__ = ("children", "word")

    def __init__(self):
        self.children = {}
        self.word = None


def solve(board, words):
    root = Node()
    for word in words:
        node = root
        for ch in word:
            node = node.children.setdefault(ch, Node())
        node.word = word

    rows, cols = len(board), len(board[0])
    found = []

    def dfs(r, c, node):
        ch = board[r][c]
        nxt = node.children.get(ch)
        if nxt is None:
            return
        if nxt.word is not None:
            found.append(nxt.word)
            nxt.word = None
        board[r][c] = "#"
        if r > 0 and board[r - 1][c] != "#":
            dfs(r - 1, c, nxt)
        if r + 1 < rows and board[r + 1][c] != "#":
            dfs(r + 1, c, nxt)
        if c > 0 and board[r][c - 1] != "#":
            dfs(r, c - 1, nxt)
        if c + 1 < cols and board[r][c + 1] != "#":
            dfs(r, c + 1, nxt)
        board[r][c] = ch

    for r in range(rows):
        for c in range(cols):
            if board[r][c] in root.children:
                dfs(r, c, root)

    found.sort()
    return found


data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx]); idx += 1
    out = []
    for _ in range(t):
        rows = int(data[idx]); cols = int(data[idx + 1]); idx += 2
        board = [list(data[idx + i]) for i in range(rows)]
        idx += rows
        w = int(data[idx]); idx += 1
        words = data[idx:idx + w]
        idx += w
        ans = solve(board, words)
        out.append("\n".join([str(len(ans))] + ans))
    sys.stdout.write("\n\n".join(out))
