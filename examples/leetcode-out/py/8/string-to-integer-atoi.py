# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def myAtoi(s: str) -> int:
	i = 0
	n = len(s)
	while ((i < n) and (s[i] == " ")):
		i = (i + 1)
	sign = 1
	if ((i < n) and (((s[i] == "+") or (s[i] == "-")))):
		if (s[i] == "-"):
			sign = (-1)
		i = (i + 1)
	digits = {"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
	result = 0
	while (i < n):
		ch = s[i]
		if (not ((ch in digits))):
			break
		d = digits[ch]
		result = ((result * 10) + d)
		i = (i + 1)
	result = (result * sign)
	if (result > 2147483647):
		return 2147483647
	if (result < ((-2147483648))):
		return (-2147483648)
	return result

def example_1():
	assert (myAtoi("42") == 42)

def example_2():
	assert (myAtoi("   -42") == ((-42)))

def example_3():
	assert (myAtoi("4193 with words") == 4193)

def example_4():
	assert (myAtoi("words and 987") == 0)

def example_5():
	assert (myAtoi("-91283472332") == ((-2147483648)))

def main():
	example_1()
	example_2()
	example_3()
	example_4()
	example_5()

if __name__ == "__main__":
	main()
