# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def isPalindrome(x: int) -> bool:
	if (x < 0):
		return False
	s = str(x)
	n = len(s)
	for i in range(0, (n // 2)):
		if (s[i] != s[((n - 1) - i)]):
			return False
	return True

def example_1():
	assert (isPalindrome(121) == True)

def example_2():
	assert (isPalindrome((-121)) == False)

def example_3():
	assert (isPalindrome(10) == False)

def zero():
	assert (isPalindrome(0) == True)

def main():
	example_1()
	example_2()
	example_3()
	zero()

if __name__ == "__main__":
	main()
