package main

import (
	"encoding/json"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func buildPrefix(matrix [][]int) [][]int {
	var rows int = len(matrix)
	var cols int = 0
	if (rows > 0) {
		cols = len(matrix[0])
	}
	var prefix [][]int = [][]int{}
	var i int = 0
	for (i <= rows) {
		var row []int = []int{}
		var j int = 0
		for (j <= cols) {
			row = append(append([]int{}, row...), []int{0}...)
			j = (j + 1)
		}
		prefix = append(append([][]int{}, prefix...), [][]int{row}...)
		i = (i + 1)
	}
	i = 1
	for (i <= rows) {
		var j int = 1
		for (j <= cols) {
			prefix[i][j] = (((matrix[(i - 1)][(j - 1)] + prefix[(i - 1)][j]) + prefix[i][(j - 1)]) - prefix[(i - 1)][(j - 1)])
			j = (j + 1)
		}
		i = (i + 1)
	}
	return prefix
}

func NumMatrix(matrix [][]int) map[string]any {
	var rows int = len(matrix)
	var cols int = 0
	if (rows > 0) {
		cols = len(matrix[0])
	}
	return map[string]any{"rows": rows, "cols": cols, "data": matrix, "prefix": buildPrefix(matrix)}
}

func numMatrixUpdate(nm map[string]any, row int, col int, val int) {
	var data [][]int = _cast[[][]int](nm["data"])
	var current int = data[row][col]
	var diff int = (val - current)
	data[row][col] = val
	nm["data"] = data
	var prefix [][]int = _cast[[][]int](nm["prefix"])
	var i int = (row + 1)
	for (i <= _cast[int](nm["rows"])) {
		var j int = (col + 1)
		for (j <= _cast[int](nm["cols"])) {
			prefix[i][j] = (prefix[i][j] + diff)
			j = (j + 1)
		}
		i = (i + 1)
	}
	nm["prefix"] = prefix
}

func numMatrixSumRegion(nm map[string]any, row1 int, col1 int, row2 int, col2 int) int {
	var p [][]int = _cast[[][]int](nm["prefix"])
	var a int = p[(row2 + 1)][(col2 + 1)]
	var b int = p[row1][(col2 + 1)]
	var c int = p[(row2 + 1)][col1]
	var d int = p[row1][col1]
	return (((a - b) - c) + d)
}

func example() {
	var nm map[string]any = NumMatrix([][]int{[]int{3, 0, 1, 4, 2}, []int{5, 6, 3, 2, 1}, []int{1, 2, 0, 1, 5}, []int{4, 1, 0, 1, 7}, []int{1, 0, 3, 0, 5}})
	expect((numMatrixSumRegion(nm, 2, 1, 4, 3) == 8))
	numMatrixUpdate(nm, 3, 2, 2)
	expect((numMatrixSumRegion(nm, 2, 1, 4, 3) == 10))
}

func single_element() {
	var nm map[string]any = NumMatrix([][]int{[]int{1}})
	expect((numMatrixSumRegion(nm, 0, 0, 0, 0) == 1))
	numMatrixUpdate(nm, 0, 0, 5)
	expect((numMatrixSumRegion(nm, 0, 0, 0, 0) == 5))
}

func main() {
	example()
	single_element()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

