# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def minDistance(word1: str, word2: str) -> int:
	m = len(word1)
	n = len(word2)
	dp = []
	i = 0
	while (i <= m):
		row = []
		j = 0
		while (j <= n):
			row = (row + [0])
			j = (j + 1)
		dp = (dp + [row])
		i = (i + 1)
	i = 0
	while (i <= m):
		dp[i][0] = i
		i = (i + 1)
	j = 0
	while (j <= n):
		dp[0][j] = j
		j = (j + 1)
	i = 1
	while (i <= m):
		j = 1
		while (j <= n):
			if (word1[(i - 1)] == word2[(j - 1)]):
				dp[i][j] = dp[(i - 1)][(j - 1)]
			else:
				insert = (dp[i][(j - 1)] + 1)
				delete = (dp[(i - 1)][j] + 1)
				replace = (dp[(i - 1)][(j - 1)] + 1)
				best = insert
				if (delete < best):
					best = delete
				if (replace < best):
					best = replace
				dp[i][j] = best
			j = (j + 1)
		i = (i + 1)
	return dp[m][n]

def example_1():
	assert (minDistance("horse", "ros") == 3)

def example_2():
	assert (minDistance("intention", "execution") == 5)

def identical_strings():
	assert (minDistance("abc", "abc") == 0)

def empty_second():
	assert (minDistance("abc", "") == 3)

def empty_first():
	assert (minDistance("", "abc") == 3)

def main():
	example_1()
	example_2()
	identical_strings()
	empty_second()
	empty_first()

if __name__ == "__main__":
	main()
