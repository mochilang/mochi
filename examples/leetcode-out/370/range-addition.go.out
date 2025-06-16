package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func getModifiedArray(n int, updates [][]int) []int {
	var diff []int = []int{}
	var i int = 0
	for (i < n) {
		diff = append(append([]int{}, diff...), []int{0}...)
		i = (i + 1)
	}
	var m int = len(updates)
	var j int = 0
	for (j < m) {
		var u []int = updates[j]
		var start int = u[0]
		var end int = u[1]
		var inc int = u[2]
		diff[start] = (diff[start] + inc)
		if ((end + 1) < n) {
			diff[(end + 1)] = (diff[(end + 1)] - inc)
		}
		j = (j + 1)
	}
	var result []int = []int{}
	var sum int = 0
	var k int = 0
	for (k < n) {
		sum = (sum + diff[k])
		result = append(append([]int{}, result...), []int{sum}...)
		k = (k + 1)
	}
	return result
}

func example_1() {
	expect((res1[0] == (-2)))
	expect((res1[1] == 0))
	expect((res1[2] == 3))
	expect((res1[3] == 5))
	expect((res1[4] == 3))
}

func no_updates() {
	expect((res2[0] == 0))
	expect((res2[1] == 0))
	expect((res2[2] == 0))
}

var updates1 [][]int = [][]int{[]int{1, 3, 2}, []int{2, 4, 3}, []int{0, 2, -2}}
var res1 []int = getModifiedArray(5, updates1)
var res2 []int = getModifiedArray(3, [][]int{})
func main() {
	example_1()
	no_updates()
}

