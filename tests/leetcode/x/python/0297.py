import collections
import sys


class TreeNode:
    def __init__(self, val: int):
        self.val = val
        self.left = None
        self.right = None


class Codec:
    def serialize(self, root):
        if root is None:
            return "[]"
        out = []
        q = collections.deque([root])
        while q:
            node = q.popleft()
            if node is None:
                out.append("null")
            else:
                out.append(str(node.val))
                q.append(node.left)
                q.append(node.right)
        while out and out[-1] == "null":
            out.pop()
        return "[" + ",".join(out) + "]"

    def deserialize(self, data):
        data = data.strip()
        if data == "[]":
            return None
        vals = data[1:-1].split(",")
        root = TreeNode(int(vals[0]))
        q = collections.deque([root])
        i = 1
        while q and i < len(vals):
            node = q.popleft()
            if i < len(vals) and vals[i] != "null":
                node.left = TreeNode(int(vals[i]))
                q.append(node.left)
            i += 1
            if i < len(vals) and vals[i] != "null":
                node.right = TreeNode(int(vals[i]))
                q.append(node.right)
            i += 1
        return root


def main() -> None:
    lines = [line.strip() for line in sys.stdin if line.strip()]
    if not lines:
        return
    t = int(lines[0])
    codec = Codec()
    out = []
    for tc in range(t):
        root = codec.deserialize(lines[tc + 1])
        if tc:
            out.append("")
        out.append(codec.serialize(root))
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
