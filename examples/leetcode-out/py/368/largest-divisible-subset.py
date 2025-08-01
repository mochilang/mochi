# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def largestDivisibleSubset(nums: list[int]) -> list[int]:
	n = len(nums)
	if (n == 0):
		return []
	_sorted = [ x for x in sorted([ x for x in nums ], key=lambda x: x) ]
	dp = []
	parent = []
	fill = 0
	while (fill < n):
		dp = (dp + [1])
		parent = (parent + [(-1)])
		fill = (fill + 1)
	maxLen = 1
	maxIdx = 0
	i = 0
	while (i < n):
		j = 0
		while (j < i):
			if ((_sorted[i] % _sorted[j]) == 0):
				candidate = (dp[j] + 1)
				if (candidate > dp[i]):
					dp[i] = candidate
					parent[i] = j
			j = (j + 1)
		if (dp[i] > maxLen):
			maxLen = dp[i]
			maxIdx = i
		i = (i + 1)
	subset = []
	k = maxIdx
	while (k >= 0):
		subset = ([_sorted[k]] + subset)
		k = parent[k]
	return subset

def example_1():
	assert (largestDivisibleSubset([1, 2, 3]) == [1, 2])

def example_2():
	assert (largestDivisibleSubset([1, 2, 4, 8]) == [1, 2, 4, 8])

def empty():
	assert (largestDivisibleSubset([]) == [])

def mixed_numbers():
	assert (largestDivisibleSubset([4, 8, 10, 240]) == [4, 8, 240])

def main():
	example_1()
	example_2()
	empty()
	mixed_numbers()

if __name__ == "__main__":
	main()
