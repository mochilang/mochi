package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func maxSumSubmatrix(matrix [][]int, k int) int {
	if ((len(matrix) == 0) || (len(matrix[0]) == 0)) {
		return 0
	}
	var rows int = len(matrix)
	var cols int = len(matrix[0])
	var best int = -2147483648
	var left int = 0
	for (left < cols) {
		var rowSums []int = []int{}
		var r int = 0
		for (r < rows) {
			rowSums = append(append([]int{}, rowSums...), []int{0}...)
			r = (r + 1)
		}
		var right int = left
		for (right < cols) {
			var i int = 0
			for (i < rows) {
				rowSums[i] = (rowSums[i] + matrix[i][right])
				i = (i + 1)
			}
			var prefixList []int = []int{0}
			var prefix int = 0
			var idx int = 0
			for (idx < rows) {
				prefix = (prefix + rowSums[idx])
				var j int = 0
				var localBest int = -2147483648
				for (j < len(prefixList)) {
					var cand int = (prefix - prefixList[j])
					if ((cand <= k) && (cand > localBest)) {
						localBest = cand
					}
					j = (j + 1)
				}
				if (localBest > best) {
					best = localBest
				}
				prefixList = append(append([]int{}, prefixList...), []int{prefix}...)
				idx = (idx + 1)
			}
			right = (right + 1)
		}
		left = (left + 1)
	}
	return best
}

func example_1() {
	var matrix [][]int = [][]int{[]int{1, 0, 1}, []int{0, -2, 3}}
	_ = matrix
	expect((maxSumSubmatrix(matrix, 2) == 2))
}

func example_2() {
	var matrix [][]int = [][]int{[]int{2, 2, -1}}
	_ = matrix
	expect((maxSumSubmatrix(matrix, 3) == 3))
}

func single_element() {
	expect((maxSumSubmatrix([][]int{[]int{5}}, 5) == 5))
}

func negative_numbers() {
	var matrix [][]int = [][]int{[]int{-1, -2}, []int{-3, -4}}
	_ = matrix
	expect((maxSumSubmatrix(matrix, (-2)) == (-2)))
}

func main() {
	example_1()
	example_2()
	single_element()
	negative_numbers()
}

