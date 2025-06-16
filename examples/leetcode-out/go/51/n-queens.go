package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func solveNQueens(n int) [][]string {
	var results [][]string = [][]string{}
	var cols map[int]bool = map[int]bool{}
	var diag1 map[int]bool = map[int]bool{}
	var diag2 map[int]bool = map[int]bool{}
	var backtrack func(int, []string)
	backtrack = func(row int, board []string) {
		if (row == n) {
			results = append(append([][]string{}, results...), [][]string{board}...)
		} else {
			var c int = 0
			for (c < n) {
				var usedCol bool = false
				_tmp0 := c
				_tmp1 := cols
				_, _tmp2 := _tmp1[_tmp0]
				if _tmp2 {
					usedCol = cols[c]
				}
				if usedCol {
					c = (c + 1)
					continue
				}
				var d1 int = (row - c)
				var d2 int = (row + c)
				var usedD1 bool = false
				var usedD2 bool = false
				_tmp3 := d1
				_tmp4 := diag1
				_, _tmp5 := _tmp4[_tmp3]
				if _tmp5 {
					usedD1 = diag1[d1]
				}
				_tmp6 := d2
				_tmp7 := diag2
				_, _tmp8 := _tmp7[_tmp6]
				if _tmp8 {
					usedD2 = diag2[d2]
				}
				if !((usedD1 || usedD2)) {
					cols[c] = true
					diag1[d1] = true
					diag2[d2] = true
					var rowStr string = ""
					var i int = 0
					for (i < n) {
						if (i == c) {
							rowStr = rowStr + "Q"
						} else {
							rowStr = rowStr + "."
						}
						i = (i + 1)
					}
					backtrack((row + 1), append(append([]string{}, board...), []string{rowStr}...))
					cols[c] = false
					diag1[d1] = false
					diag2[d2] = false
				}
				c = (c + 1)
			}
		}
}
	backtrack(0, []string{})
	return results
}

func n_4() {
	expect(_equal(result4, expected4))
}

func n_1() {
	expect(_equal(solveNQueens(1), [][]string{[]string{"Q"}}))
}

var result4 [][]string = solveNQueens(4)
var expected4 [][]string = [][]string{[]string{".Q..", "...Q", "Q...", "..Q."}, []string{"..Q.", "Q...", "...Q", ".Q.."}}
func main() {
	n_4()
	n_1()
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

