# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def longestConsecutive(nums: list[int]) -> int:
	_set = {}
	for n in nums:
		_set[n] = True
	best = 0
	for n in _set:
		if (not (((n - 1) in _set))):
			curr = n
			length = 1
			while ((curr + 1) in _set):
				curr = (curr + 1)
				length = (length + 1)
			if (length > best):
				best = length
	return best

def example_1():
	assert (longestConsecutive([100, 4, 200, 1, 3, 2]) == 4)

def example_2():
	assert (longestConsecutive([0, 3, 7, 2, 5, 8, 4, 6, 0, 1]) == 9)

def empty():
	assert (longestConsecutive([]) == 0)

def duplicates():
	assert (longestConsecutive([1, 2, 0, 1]) == 3)

def main():
	example_1()
	example_2()
	empty()
	duplicates()

if __name__ == "__main__":
	main()
