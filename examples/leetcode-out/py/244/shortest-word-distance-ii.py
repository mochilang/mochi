# Generated by Mochi Python compiler
from __future__ import annotations

import dataclasses
import typing

def newWordDistance(words: list[str]) -> WordDistance:
	m = {}
	i = 0
	while (i < len(words)):
		w = words[i]
		lst = []
		if (w in m):
			lst = m[w]
		lst = (lst + [i])
		m[w] = lst
		i = (i + 1)
	return WordDistance(index=m)

def min(a: int, b: int) -> int:
	if (a < b):
		return a
	return b

def shortest(wd: WordDistance, word1: str, word2: str) -> int:
	list1 = wd.index[word1]
	list2 = wd.index[word2]
	i = 0
	j = 0
	best = 1000000000
	while ((i < len(list1)) and (j < len(list2))):
		a = list1[i]
		b = list2[j]
		diff = (a - b)
		if (diff < 0):
			diff = (-diff)
		best = min(best, diff)
		if (a < b):
			i = (i + 1)
		else:
			j = (j + 1)
	return best

@dataclasses.dataclass
class WordDistance:
	index: dict[str, list[int]]

def example():
	words = ["practice", "makes", "perfect", "coding", "makes"]
	wd = newWordDistance(words)
	assert (shortest(wd, "coding", "practice") == 3)
	assert (shortest(wd, "makes", "coding") == 1)

def same_word_many_times():
	words = ["a", "a", "b", "a"]
	wd = newWordDistance(words)
	assert (shortest(wd, "a", "b") == 1)

def main():
	example()
	same_word_many_times()

if __name__ == "__main__":
	main()
