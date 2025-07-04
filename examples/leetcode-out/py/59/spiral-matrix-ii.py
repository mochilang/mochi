# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def generateMatrix(n: int) -> list[list[int]]:
	matrix = []
	i = 0
	while (i < n):
		row = []
		j = 0
		while (j < n):
			row = (row + [0])
			j = (j + 1)
		matrix = (matrix + [row])
		i = (i + 1)
	left = 0
	right = (n - 1)
	top = 0
	bottom = (n - 1)
	num = 1
	while ((left <= right) and (top <= bottom)):
		for j in range(left, (right + 1)):
			matrix[top][j] = num
			num = (num + 1)
		top = (top + 1)
		for i in range(top, (bottom + 1)):
			matrix[i][right] = num
			num = (num + 1)
		right = (right - 1)
		if (top <= bottom):
			j = right
			while (j >= left):
				matrix[bottom][j] = num
				num = (num + 1)
				j = (j - 1)
			bottom = (bottom - 1)
		if (left <= right):
			i = bottom
			while (i >= top):
				matrix[i][left] = num
				num = (num + 1)
				i = (i - 1)
			left = (left + 1)
	return matrix

def example_1():
	assert (generateMatrix(3) == [[1, 2, 3], [8, 9, 4], [7, 6, 5]])

def example_2():
	assert (generateMatrix(1) == [[1]])

def n___4():
	assert (generateMatrix(4) == [[1, 2, 3, 4], [12, 13, 14, 5], [11, 16, 15, 6], [10, 9, 8, 7]])

def main():
	example_1()
	example_2()
	n___4()

if __name__ == "__main__":
	main()
