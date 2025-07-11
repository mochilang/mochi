# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def wallsAndGates(rooms: list[list[int]]) -> list[list[int]]:
	rows = len(rooms)
	if (rows == 0):
		return rooms
	cols = len(rooms[0])
	queue = []
	for r in range(0, rows):
		for c in range(0, cols):
			if (rooms[r][c] == 0):
				queue = (queue + [[r, c]])
	idx = 0
	while (idx < len(queue)):
		pos = queue[idx]
		idx = (idx + 1)
		r = pos[0]
		c = pos[1]
		dist = rooms[r][c]
		if (r > 0):
			if (rooms[(r - 1)][c] == 2147483647):
				rooms[(r - 1)][c] = (dist + 1)
				queue = (queue + [[(r - 1), c]])
		if ((r + 1) < rows):
			if (rooms[(r + 1)][c] == 2147483647):
				rooms[(r + 1)][c] = (dist + 1)
				queue = (queue + [[(r + 1), c]])
		if (c > 0):
			if (rooms[r][(c - 1)] == 2147483647):
				rooms[r][(c - 1)] = (dist + 1)
				queue = (queue + [[r, (c - 1)]])
		if ((c + 1) < cols):
			if (rooms[r][(c + 1)] == 2147483647):
				rooms[r][(c + 1)] = (dist + 1)
				queue = (queue + [[r, (c + 1)]])
	return rooms

def example():
	INF = 2147483647
	rooms = [[INF, (-1), 0, INF], [INF, INF, INF, (-1)], [INF, (-1), INF, (-1)], [0, (-1), INF, INF]]
	expected = [[3, (-1), 0, 1], [2, 2, 1, (-1)], [1, (-1), 2, (-1)], [0, (-1), 3, 4]]
	assert (wallsAndGates(rooms) == expected)

def all_walls():
	rooms = [[(-1), (-1)], [(-1), (-1)]]
	assert (wallsAndGates(rooms) == rooms)

def single_gate():
	rooms = [[2147483647, 0, 2147483647]]
	expected = [[1, 0, 1]]
	assert (wallsAndGates(rooms) == expected)

def empty():
	rooms = []
	assert (wallsAndGates(rooms) == rooms)

def main():
	example()
	all_walls()
	single_gate()
	empty()

if __name__ == "__main__":
	main()
