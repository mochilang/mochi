# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def maxEnvelopes(envelopes: list[list[int]]) -> int:
	n = len(envelopes)
	if (n == 0):
		return 0
	byH = [ e for e in sorted([ e for e in envelopes ], key=lambda e: (-e[1])) ]
	_sorted = [ e for e in sorted([ e for e in byH ], key=lambda e: e[0]) ]
	tails = []
	fill = 0
	while (fill < n):
		tails = (tails + [0])
		fill = (fill + 1)
	size = 0
	i = 0
	while (i < n):
		height = _sorted[i][1]
		lo = 0
		hi = size
		while (lo < hi):
			mid = (((lo + hi)) // 2)
			if (tails[mid] < height):
				lo = (mid + 1)
			else:
				hi = mid
		tails[lo] = height
		if (lo == size):
			size = (size + 1)
		i = (i + 1)
	return size

def example_1():
	assert (maxEnvelopes([[5, 4], [6, 4], [6, 7], [2, 3]]) == 3)

def example_2():
	assert (maxEnvelopes([[1, 1], [1, 1], [1, 1]]) == 1)

def empty():
	assert (maxEnvelopes([]) == 0)

def increasing():
	assert (maxEnvelopes([[1, 1], [2, 2], [3, 3], [4, 4]]) == 4)

def main():
	example_1()
	example_2()
	empty()
	increasing()

if __name__ == "__main__":
	main()
