# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def parseInt(s: str) -> int:
	result = 0
	i = 0
	digits = {"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	while (i < len(s)):
		result = ((result * 10) + digits[s[i]])
		i = (i + 1)
	return result

def addOperators(num: str, target: int) -> list[str]:
	result = []
	def backtrack(pos: int, expr: str, value: int, prev: int) -> None:
		nonlocal result
		if (pos == len(num)):
			if (value == target):
				result = (result + [expr])
		else:
			i = pos
			while (i < len(num)):
				if ((i != pos) and (num[pos] == "0")):
					break
				part = num[pos:(i + 1)]
				cur = parseInt(part)
				if (pos == 0):
					backtrack((i + 1), part, cur, cur)
				else:
					backtrack((i + 1), ((expr + "+") + part), (value + cur), cur)
					backtrack((i + 1), ((expr + "-") + part), (value - cur), (-cur))
					backtrack((i + 1), ((expr + "*") + part), ((value - prev) + (prev * cur)), (prev * cur))
				i = (i + 1)
	backtrack(0, "", 0, 0)
	return result

def example_1():
	res = addOperators("123", 6)
	_sorted = [ x for x in sorted([ x for x in res ], key=lambda x: x) ]
	assert (_sorted == ["1*2*3", "1+2+3"])

def example_2():
	res = addOperators("232", 8)
	_sorted = [ x for x in sorted([ x for x in res ], key=lambda x: x) ]
	assert (_sorted == ["2*3+2", "2+3*2"])

def example_3():
	res = addOperators("105", 5)
	_sorted = [ x for x in sorted([ x for x in res ], key=lambda x: x) ]
	assert (_sorted == ["1*0+5", "10-5"])

def example_4():
	res = addOperators("00", 0)
	_sorted = [ x for x in sorted([ x for x in res ], key=lambda x: x) ]
	assert (_sorted == ["0*0", "0+0", "0-0"])

def example_5():
	assert (addOperators("3456237490", 9191) == [])

def single_number():
	assert (addOperators("5", 5) == ["5"])

def main():
	example_1()
	example_2()
	example_3()
	example_4()
	example_5()
	single_number()

if __name__ == "__main__":
	main()
