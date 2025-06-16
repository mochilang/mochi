package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isValidSudoku(board [][]string) bool {
	for r := 0; r < 9; r++ {
		var seen map[string]bool = map[string]bool{}
		for c := 0; c < 9; c++ {
			var val string = board[r][c]
			if (val != ".") {
				_tmp0 := val
				_tmp1 := seen
				_, _tmp2 := _tmp1[_tmp0]
				if _tmp2 {
					return false
				}
				seen[val] = true
			}
		}
	}
	for c := 0; c < 9; c++ {
		var seen map[string]bool = map[string]bool{}
		for r := 0; r < 9; r++ {
			var val string = board[r][c]
			if (val != ".") {
				_tmp3 := val
				_tmp4 := seen
				_, _tmp5 := _tmp4[_tmp3]
				if _tmp5 {
					return false
				}
				seen[val] = true
			}
		}
	}
	for br := 0; br < 3; br++ {
		for bc := 0; bc < 3; bc++ {
			var seen map[string]bool = map[string]bool{}
			for r := 0; r < 3; r++ {
				for c := 0; c < 3; c++ {
					var val string = board[((br * 3) + r)][((bc * 3) + c)]
					if (val != ".") {
						_tmp6 := val
						_tmp7 := seen
						_, _tmp8 := _tmp7[_tmp6]
						if _tmp8 {
							return false
						}
						seen[val] = true
					}
				}
			}
		}
	}
	return true
}

func example_1() {
	expect((isValidSudoku(example1) == true))
}

func example_2() {
	expect((isValidSudoku(example2) == false))
}

var example1 [][]string = [][]string{[]string{"5", "3", ".", ".", "7", ".", ".", ".", "."}, []string{"6", ".", ".", "1", "9", "5", ".", ".", "."}, []string{".", "9", "8", ".", ".", ".", ".", "6", "."}, []string{"8", ".", ".", ".", "6", ".", ".", ".", "3"}, []string{"4", ".", ".", "8", ".", "3", ".", ".", "1"}, []string{"7", ".", ".", ".", "2", ".", ".", ".", "6"}, []string{".", "6", ".", ".", ".", ".", "2", "8", "."}, []string{".", ".", ".", "4", "1", "9", ".", ".", "5"}, []string{".", ".", ".", ".", "8", ".", ".", "7", "9"}}
var example2 [][]string = [][]string{[]string{"8", "3", ".", ".", "7", ".", ".", ".", "."}, []string{"6", ".", ".", "1", "9", "5", ".", ".", "."}, []string{".", "9", "8", ".", ".", ".", ".", "6", "."}, []string{"8", ".", ".", ".", "6", ".", ".", ".", "3"}, []string{"4", ".", ".", "8", ".", "3", ".", ".", "1"}, []string{"7", ".", ".", ".", "2", ".", ".", ".", "6"}, []string{".", "6", ".", ".", ".", ".", "2", "8", "."}, []string{".", ".", ".", "4", "1", "9", ".", ".", "5"}, []string{".", ".", ".", ".", "8", ".", ".", "7", "9"}}
func main() {
	example_1()
	example_2()
}

