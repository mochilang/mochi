package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func countPrimes(n int) int {
	if (n <= 2) {
		return 0
	}
	var isPrime []bool = []bool{}
	var i int = 0
	for (i < n) {
		isPrime = append(append([]bool{}, isPrime...), []bool{true}...)
		i = (i + 1)
	}
	isPrime[0] = false
	isPrime[1] = false
	var p int = 2
	for ((p * p) < n) {
		if isPrime[p] {
			var j int = (p * p)
			for (j < n) {
				isPrime[j] = false
				j = (j + p)
			}
		}
		p = (p + 1)
	}
	var count int = 0
	var k int = 0
	for (k < n) {
		if isPrime[k] {
			count = (count + 1)
		}
		k = (k + 1)
	}
	return count
}

func example_1() {
	expect((countPrimes(10) == 4))
}

func example_2() {
	expect((countPrimes(0) == 0))
}

func example_3() {
	expect((countPrimes(1) == 0))
}

func primes_under_20() {
	expect((countPrimes(20) == 8))
}

func larger_input() {
	expect((countPrimes(100) == 25))
}

func main() {
	example_1()
	example_2()
	example_3()
	primes_under_20()
	larger_input()
}

