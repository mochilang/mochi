# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def detectCycle(_next: list[int]) -> int:
	if (len(_next) == 0):
		return (-1)
	slow = 0
	fast = 0
	while ((fast != ((-1))) and (_next[fast] != ((-1)))):
		slow = _next[slow]
		fast = _next[_next[fast]]
		if (slow == fast):
			break
	if ((fast == ((-1))) or (_next[fast] == ((-1)))):
		return (-1)
	start = 0
	while (start != slow):
		start = _next[start]
		slow = _next[slow]
	return start

def example_1():
	assert (detectCycle([1, 2, 3, 1]) == 1)

def no_cycle():
	assert (detectCycle([1, 2, 3, (-1)]) == ((-1)))

def cycle_at_head():
	assert (detectCycle([0]) == 0)

def single_node_no_cycle():
	assert (detectCycle([(-1)]) == ((-1)))

def main():
	example_1()
	no_cycle()
	cycle_at_head()
	single_node_no_cycle()

if __name__ == "__main__":
	main()
