# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def trailingZeroes(n: int) -> int:
	count = 0
	divisor = 5
	while (divisor <= n):
		count = (count + ((n // divisor)))
		divisor = (divisor * 5)
	return count

def example_1():
	assert (trailingZeroes(3) == 0)

def example_2():
	assert (trailingZeroes(5) == 1)

def example_3():
	assert (trailingZeroes(0) == 0)

def large_value():
	assert (trailingZeroes(30) == 7)

def main():
	example_1()
	example_2()
	example_3()
	large_value()

if __name__ == "__main__":
	main()
