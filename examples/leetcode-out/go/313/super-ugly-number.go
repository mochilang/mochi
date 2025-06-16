package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func nthSuperUglyNumber(n int, primes []int) int {
	var k int = len(primes)
	var indices []int = []int{}
	var t int = 0
	for (t < k) {
		indices = append(append([]int{}, indices...), []int{0}...)
		t = (t + 1)
	}
	var uglies []int = []int{1}
	var count int = 1
	for (count < n) {
		var next int = 1000000000
		var i int = 0
		for (i < k) {
			var candidate int = (primes[i] * uglies[indices[i]])
			if (candidate < next) {
				next = candidate
			}
			i = (i + 1)
		}
		uglies = append(append([]int{}, uglies...), []int{next}...)
		var j int = 0
		for (j < k) {
			if ((primes[j] * uglies[indices[j]]) == next) {
				indices[j] = (indices[j] + 1)
			}
			j = (j + 1)
		}
		count = (count + 1)
	}
	return uglies[(n - 1)]
}

func example_1() {
	expect((nthSuperUglyNumber(12, []int{2, 7, 13, 19}) == 32))
}

func example_2() {
	expect((nthSuperUglyNumber(1, []int{2, 3, 5}) == 1))
}

func small_primes() {
	expect((nthSuperUglyNumber(3, []int{2, 3, 5}) == 3))
}

func main() {
	example_1()
	example_2()
	small_primes()
}

