# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def numIslands(grid: list[list[str]]) -> int:
	rows = len(grid)
	if (rows == 0):
		return 0
	cols = len(grid[0])
	visited = []
	r = 0
	while (r < rows):
		row = []
		c = 0
		while (c < cols):
			row = (row + [False])
			c = (c + 1)
		visited = (visited + [row])
		r = (r + 1)
	def dfs(i: int, j: int) -> int:
		nonlocal visited
		if ((((i < 0) or (i >= rows)) or (j < 0)) or (j >= cols)):
			return 0
		if visited[i][j]:
			return 0
		if (grid[i][j] != "1"):
			return 0
		visited[i][j] = True
		dfs((i + 1), j)
		dfs((i - 1), j)
		dfs(i, (j + 1))
		dfs(i, (j - 1))
		return 0
	count = 0
	r = 0
	while (r < rows):
		c = 0
		while (c < cols):
			if (grid[r][c] == "1"):
				if (not (visited[r][c])):
					dfs(r, c)
					count = (count + 1)
			c = (c + 1)
		r = (r + 1)
	return count

grid1 = [["1", "1", "1", "1", "0"], ["1", "1", "0", "1", "0"], ["1", "1", "0", "0", "0"], ["0", "0", "0", "0", "0"]]
grid2 = [["1", "1", "0", "0", "0"], ["1", "1", "0", "0", "0"], ["0", "0", "1", "0", "0"], ["0", "0", "0", "1", "1"]]

def example_1():
	assert (numIslands(grid1) == 1)

def example_2():
	assert (numIslands(grid2) == 3)

def empty_grid():
	assert (numIslands([]) == 0)

def all_water():
	assert (numIslands([["0", "0"], ["0", "0"]]) == 0)

def single_island():
	assert (numIslands([["1"]]) == 1)

def main():
	grid1 = [["1", "1", "1", "1", "0"], ["1", "1", "0", "1", "0"], ["1", "1", "0", "0", "0"], ["0", "0", "0", "0", "0"]]
	grid2 = [["1", "1", "0", "0", "0"], ["1", "1", "0", "0", "0"], ["0", "0", "1", "0", "0"], ["0", "0", "0", "1", "1"]]
	example_1()
	example_2()
	empty_grid()
	all_water()
	single_island()

if __name__ == "__main__":
	main()
