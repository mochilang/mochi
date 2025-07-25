# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def mergeSorted(a: list[int], b: list[int]) -> list[int]:
	i = 0
	j = 0
	result = []
	while ((i < len(a)) and (j < len(b))):
		if (a[i] <= b[j]):
			result = (result + [a[i]])
			i = (i + 1)
		else:
			result = (result + [b[j]])
			j = (j + 1)
	while (i < len(a)):
		result = (result + [a[i]])
		i = (i + 1)
	while (j < len(b)):
		result = (result + [b[j]])
		j = (j + 1)
	return result

def sortList(nums: list[int]) -> list[int]:
	if (len(nums) <= 1):
		return nums
	mid = (len(nums) // 2)
	left = sortList(nums[0:mid])
	right = sortList(nums[mid:len(nums)])
	return mergeSorted(left, right)

def example_1():
	assert (sortList([4, 2, 1, 3]) == [1, 2, 3, 4])

def example_2():
	assert (sortList([(-1), 5, 3, 4, 0]) == [(-1), 0, 3, 4, 5])

def single():
	assert (sortList([1]) == [1])

def empty():
	assert (sortList([]) == [])

def main():
	example_1()
	example_2()
	single()
	empty()

if __name__ == "__main__":
	main()
