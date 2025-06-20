# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def max(a: int, b: int) -> int:
	if (a > b):
		return a
	return b

def min(a: int, b: int) -> int:
	if (a < b):
		return a
	return b

def computeArea(ax1: int, ay1: int, ax2: int, ay2: int, bx1: int, by1: int, bx2: int, by2: int) -> int:
	areaA = (((ax2 - ax1)) * ((ay2 - ay1)))
	areaB = (((bx2 - bx1)) * ((by2 - by1)))
	overlapWidth = (min(ax2, bx2) - max(ax1, bx1))
	overlapHeight = (min(ay2, by2) - max(ay1, by1))
	overlap = 0
	if ((overlapWidth > 0) and (overlapHeight > 0)):
		overlap = (overlapWidth * overlapHeight)
	return ((areaA + areaB) - overlap)

def example_1():
	assert (computeArea((-3), 0, 3, 4, 0, (-1), 9, 2) == 45)

def example_2():
	assert (computeArea((-2), (-2), 2, 2, (-2), (-2), 2, 2) == 16)

def no_overlap():
	assert (computeArea((-1), (-1), 1, 1, 2, 2, 3, 3) == 5)

def touching_edges():
	assert (computeArea(0, 0, 1, 1, 1, 0, 2, 1) == 2)

def one_inside_another():
	assert (computeArea((-2), (-2), 2, 2, (-1), (-1), 1, 1) == 16)

def main():
	example_1()
	example_2()
	no_overlap()
	touching_edges()
	one_inside_another()

if __name__ == "__main__":
	main()
