package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func setZeroes(matrix [][]int) [][]int {
	var rows int = len(matrix)
	if (rows == 0) {
		return matrix
	}
	var cols int = len(matrix[0])
	var zeroRows []bool = []bool{}
	var zeroCols []bool = []bool{}
	var r int = 0
	for (r < rows) {
		zeroRows = append(append([]bool{}, zeroRows...), []bool{false}...)
		r = (r + 1)
	}
	var c int = 0
	for (c < cols) {
		zeroCols = append(append([]bool{}, zeroCols...), []bool{false}...)
		c = (c + 1)
	}
	var i int = 0
	for (i < rows) {
		var j int = 0
		for (j < cols) {
			if (matrix[i][j] == 0) {
				zeroRows[i] = true
				zeroCols[j] = true
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	i = 0
	for (i < rows) {
		var j int = 0
		for (j < cols) {
			if (zeroRows[i] || zeroCols[j]) {
				matrix[i][j] = 0
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return matrix
}

func example_1() {
	var m [][]int = [][]int{[]int{1, 1, 1}, []int{1, 0, 1}, []int{1, 1, 1}}
	setZeroes(m)
	expect(_equal(m, [][]int{[]int{1, 0, 1}, []int{0, 0, 0}, []int{1, 0, 1}}))
}

func example_2() {
	var m [][]int = [][]int{[]int{0, 1, 2, 0}, []int{3, 4, 5, 2}, []int{1, 3, 1, 5}}
	setZeroes(m)
	expect(_equal(m, [][]int{[]int{0, 0, 0, 0}, []int{0, 4, 5, 0}, []int{0, 3, 1, 0}}))
}

func main() {
	example_1()
	example_2()
}

func _equal(a, b any) bool {
    av := reflect.ValueOf(a)
    bv := reflect.ValueOf(b)
    if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {
        if av.Len() != bv.Len() { return false }
        for i := 0; i < av.Len(); i++ {
            if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) { return false }
        }
        return true
    }
    return reflect.DeepEqual(a, b)
}

