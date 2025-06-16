package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func candy(ratings []int) int {
	var n int = len(ratings)
	if (n == 0) {
		return 0
	}
	var candies []int = []int{}
	var i int = 0
	for (i < n) {
		candies = append(append([]int{}, candies...), []int{1}...)
		i = (i + 1)
	}
	i = 1
	for (i < n) {
		if (ratings[i] > ratings[(i - 1)]) {
			candies[i] = (candies[(i - 1)] + 1)
		}
		i = (i + 1)
	}
	var j int = (n - 2)
	for (j >= 0) {
		if ((ratings[j] > ratings[(j + 1)]) && (candies[j] <= candies[(j + 1)])) {
			candies[j] = (candies[(j + 1)] + 1)
		}
		j = (j - 1)
	}
	var total int = 0
	for _, c := range candies {
		total = (total + c)
	}
	return total
}

func example_1() {
	expect((candy([]int{1, 0, 2}) == 5))
}

func example_2() {
	expect((candy([]int{1, 2, 2}) == 4))
}

func all_equal() {
	expect((candy([]int{1, 1, 1}) == 3))
}

func descending() {
	expect((candy([]int{5, 4, 3, 2, 1}) == 15))
}

func main() {
	example_1()
	example_2()
	all_equal()
	descending()
}

