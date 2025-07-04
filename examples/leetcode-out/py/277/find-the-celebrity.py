# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def findCelebrity(mat: list[list[bool]]) -> int:
	n = len(mat)
	def knows(a: int, b: int) -> bool:
		return mat[a][b]
	candidate = 0
	i = 1
	while (i < n):
		if knows(candidate, i):
			candidate = i
		i = (i + 1)
	j = 0
	while (j < n):
		if (j != candidate):
			if knows(candidate, j):
				return (-1)
			if (not knows(j, candidate)):
				return (-1)
		j = (j + 1)
	return candidate

def example_1():
	mat = [[False, True], [False, False]]
	assert (findCelebrity(mat) == 1)

def example_2():
	mat = [[False, True, False], [False, False, False], [True, True, False]]
	assert (findCelebrity(mat) == 1)

def no_celebrity():
	mat = [[False, True], [True, False]]
	assert (findCelebrity(mat) == ((-1)))

def main():
	example_1()
	example_2()
	no_celebrity()

if __name__ == "__main__":
	main()
