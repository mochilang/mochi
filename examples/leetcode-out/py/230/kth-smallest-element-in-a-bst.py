# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def Leaf() -> dict[str, typing.Any]:
	return {"__name": "Leaf"}

def Node(left: dict[str, typing.Any], value: int, right: dict[str, typing.Any]) -> dict[str, typing.Any]:
	return {"__name": "Node", "left": left, "value": value, "right": right}

def isLeaf(t: dict[str, typing.Any]) -> bool:
	return (t["__name"] == "Leaf")

def left(t: dict[str, typing.Any]) -> dict[str, typing.Any]:
	return t["left"]

def right(t: dict[str, typing.Any]) -> dict[str, typing.Any]:
	return t["right"]

def value(t: dict[str, typing.Any]) -> int:
	return t["value"]

def kthSmallest(root: dict[str, typing.Any], k: int) -> int:
	stack = []
	curr = root
	count = 0
	while ((not isLeaf(curr)) or (len(stack) > 0)):
		while (not isLeaf(curr)):
			stack = (stack + [curr])
			curr = left(curr)
		node = stack[(len(stack) - 1)]
		stack = stack[0:(len(stack) - 1)]
		count = (count + 1)
		if (count == k):
			return value(node)
		curr = right(node)
	return 0

example1 = Node(Node(Leaf(), 1, Node(Leaf(), 2, Leaf())), 3, Node(Leaf(), 4, Leaf()))

def example_1():
	assert (kthSmallest(example1, 1) == 1)

def example_2():
	assert (kthSmallest(example2, 3) == 3)

def single_node():
	assert (kthSmallest(Node(Leaf(), 8, Leaf()), 1) == 8)

def k_equals_number_of_nodes():
	assert (kthSmallest(example1, 4) == 4)

def main():
	example1 = Node(Node(Leaf(), 1, Node(Leaf(), 2, Leaf())), 3, Node(Leaf(), 4, Leaf()))
	example2 = Node(Node(Node(Leaf(), 2, Node(Leaf(), 1, Leaf())), 3, Node(Leaf(), 4, Leaf())), 5, Node(Leaf(), 6, Leaf()))
	example_1()
	example_2()
	single_node()
	k_equals_number_of_nodes()

if __name__ == "__main__":
	main()
