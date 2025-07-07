from dataclasses import dataclass
from typing import Union

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

def sum_tree(t: Tree) -> int:
    if isinstance(t, Leaf):
        return 0
    elif isinstance(t, Node):
        return sum_tree(t.left) + t.value + sum_tree(t.right)
    else:
        raise TypeError("Unknown node")

t = Node(Leaf(), 1, Node(Leaf(), 2, Leaf()))
print(sum_tree(t))
