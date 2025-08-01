# Generated by Mochi Python compiler
from __future__ import annotations

import typing

def gcd(a: int, b: int) -> int:
	x = a
	if (x < 0):
		x = (-x)
	y = b
	if (y < 0):
		y = (-y)
	while (y != 0):
		temp = (x % y)
		x = y
		y = temp
	return x

def canMeasureWater(jug1Capacity: int, jug2Capacity: int, targetCapacity: int) -> bool:
	if (targetCapacity == 0):
		return True
	if ((jug1Capacity + jug2Capacity) < targetCapacity):
		return False
	if (((jug1Capacity == targetCapacity) or (jug2Capacity == targetCapacity)) or ((jug1Capacity + jug2Capacity) == targetCapacity)):
		return True
	g = gcd(jug1Capacity, jug2Capacity)
	return ((targetCapacity % g) == 0)

def example_1():
	assert (canMeasureWater(3, 5, 4) == True)

def example_2():
	assert (canMeasureWater(2, 6, 5) == False)

def example_3():
	assert (canMeasureWater(1, 2, 3) == True)

def zero_target():
	assert (canMeasureWater(1, 2, 0) == True)

def unreachable():
	assert (canMeasureWater(1, 1, 12) == False)

def main():
	example_1()
	example_2()
	example_3()
	zero_target()
	unreachable()

if __name__ == "__main__":
	main()
