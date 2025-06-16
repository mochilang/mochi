package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func gcd(a int, b int) int {
	var x int = a
	if (x < 0) {
		x = -x
	}
	var y int = b
	if (y < 0) {
		y = -y
	}
	for (y != 0) {
		var temp int = (x % y)
		x = y
		y = temp
	}
	return x
}

func canMeasureWater(jug1Capacity int, jug2Capacity int, targetCapacity int) bool {
	if (targetCapacity == 0) {
		return true
	}
	if ((jug1Capacity + jug2Capacity) < targetCapacity) {
		return false
	}
	if (((jug1Capacity == targetCapacity) || (jug2Capacity == targetCapacity)) || ((jug1Capacity + jug2Capacity) == targetCapacity)) {
		return true
	}
	var g int = gcd(jug1Capacity, jug2Capacity)
	return ((targetCapacity % g) == 0)
}

func example_1() {
	expect((canMeasureWater(3, 5, 4) == true))
}

func example_2() {
	expect((canMeasureWater(2, 6, 5) == false))
}

func example_3() {
	expect((canMeasureWater(1, 2, 3) == true))
}

func zero_target() {
	expect((canMeasureWater(1, 2, 0) == true))
}

func unreachable() {
	expect((canMeasureWater(1, 1, 12) == false))
}

func main() {
	example_1()
	example_2()
	example_3()
	zero_target()
	unreachable()
}

