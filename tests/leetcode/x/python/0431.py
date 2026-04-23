import sys
from collections import deque

class Node:
    def __init__(self, val: int):
        self.val = val
        self.children: list[Node] = []

class TreeNode:
    def __init__(self, val: int):
        self.val = val
        self.left: TreeNode | None = None
        self.right: TreeNode | None = None

class Codec:
    def encode(self, root: Node | None) -> TreeNode | None:
        if root is None:
            return None
        b = TreeNode(root.val)
        prev: TreeNode | None = None
        for child in root.children:
            enc = self.encode(child)
            if prev is None:
                b.left = enc
            else:
                prev.right = enc
            prev = enc
        return b

    def decode(self, root: TreeNode | None) -> Node | None:
        if root is None:
            return None
        node = Node(root.val)
        child = root.left
        while child is not None:
            decoded = self.decode(child)
            assert decoded is not None
            node.children.append(decoded)
            child = child.right
        return node

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
        ans.append(format_level(codec.decode(codec.encode(root))))
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
