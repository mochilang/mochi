from dataclasses import dataclass

class Tree:
    pass
@dataclass
class Leaf(Tree):
    pass
@dataclass
class Node(Tree):
    left: Tree
    value: int
    right: Tree

def sum_tree(t):
    return (0 if isinstance(t, Leaf) else ((lambda left, value, right: sum_tree(t.left) + t.value + sum_tree(t.right))(t.left, t.value, t.right) if isinstance(t, Node) else None))
t = Node(left=Leaf(), value=1, right=Node(left=Leaf(), value=2, right=Leaf()))
print(sum_tree(t))
