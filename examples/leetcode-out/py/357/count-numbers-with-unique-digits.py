# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def countNumbersWithUniqueDigits(n: int) -> int:
	if (n == 0):
		return 1
	if (n > 10):
		n = 10
	result = 10
	unique = 9
	available = 9
	i = 2
	while (i <= n):
		unique = (unique * available)
		result = (result + unique)
		available = (available - 1)
		i = (i + 1)
	return result

def example_1():
	assert (countNumbersWithUniqueDigits(2) == 91)

def example_2():
	assert (countNumbersWithUniqueDigits(0) == 1)

def three_digits():
	assert (countNumbersWithUniqueDigits(3) == 739)

def up_to_ten_digits():
	assert (countNumbersWithUniqueDigits(10) == 8877691)

def more_than_ten():
	assert (countNumbersWithUniqueDigits(11) == 8877691)

def main():
	example_1()
	example_2()
	three_digits()
	up_to_ten_digits()
	more_than_ten()

if __name__ == "__main__":
	main()
