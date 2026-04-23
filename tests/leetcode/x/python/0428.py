import sys
from collections import deque

class Node:
    def __init__(self, val: int):
        self.val = val
        self.children: list[Node] = []

class Codec:
    def serialize(self, root: Node | None) -> str:
        vals: list[str] = []
        def dfs(node: Node) -> None:
            vals.append(str(node.val))
            vals.append(str(len(node.children)))
            for child in node.children:
                dfs(child)
        if root is not None:
            dfs(root)
        return ' '.join(vals)

    def deserialize(self, data: str) -> Node | None:
        if not data:
            return None
        vals = data.split()
        idx = 0
        def dfs() -> Node:
            nonlocal idx
            node = Node(int(vals[idx])); idx += 1
            count = int(vals[idx]); idx += 1
            for _ in range(count):
                node.children.append(dfs())
            return node
        return dfs()

def parse_level(s: str) -> Node | None:
    if s == '[]':
        return None
    toks = s[1:-1].split(',')
    root = Node(int(toks[0]))
    q: deque[Node] = deque([root])
    i = 1
    if i < len(toks) and toks[i] == 'null':
        i += 1
    while q and i < len(toks):
        parent = q.popleft()
        while i < len(toks) and toks[i] != 'null':
            child = Node(int(toks[i]))
            parent.children.append(child)
            q.append(child)
            i += 1
        if i < len(toks) and toks[i] == 'null':
            i += 1
    return root

def format_level(root: Node | None) -> str:
    if root is None:
        return '[]'
    out = [str(root.val), 'null']
    q: deque[Node] = deque([root])
    while q:
        node = q.popleft()
        for child in node.children:
            out.append(str(child.val))
            q.append(child)
        out.append('null')
    while out and out[-1] == 'null':
        out.pop()
    return '[' + ','.join(out) + ']'

def main() -> None:
    lines = [line.strip() for line in sys.stdin.read().splitlines() if line.strip()]
    if not lines:
        return
    codec = Codec()
    ans: list[str] = []
    for s in lines[1:1 + int(lines[0])]:
        root = parse_level(s)
        ans.append(format_level(codec.deserialize(codec.serialize(root))))
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
