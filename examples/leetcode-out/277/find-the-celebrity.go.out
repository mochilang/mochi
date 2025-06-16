package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func findCelebrity(mat [][]bool) int {
	var n int = len(mat)
	var knows = func(a int, b int) bool {
		return mat[a][b]
}
	var candidate int = 0
	var i int = 1
	for (i < n) {
		if knows(candidate, i) {
			candidate = i
		}
		i = (i + 1)
	}
	var j int = 0
	for (j < n) {
		if (j != candidate) {
			if knows(candidate, j) {
				return -1
			}
			if !knows(j, candidate) {
				return -1
			}
		}
		j = (j + 1)
	}
	return candidate
}

func example_1() {
	var mat [][]bool = [][]bool{[]bool{false, true}, []bool{false, false}}
	_ = mat
	expect((findCelebrity(mat) == 1))
}

func example_2() {
	var mat [][]bool = [][]bool{[]bool{false, true, false}, []bool{false, false, false}, []bool{true, true, false}}
	_ = mat
	expect((findCelebrity(mat) == 1))
}

func no_celebrity() {
	var mat [][]bool = [][]bool{[]bool{false, true}, []bool{true, false}}
	_ = mat
	expect((findCelebrity(mat) == (-1)))
}

func main() {
	example_1()
	example_2()
	no_celebrity()
}

