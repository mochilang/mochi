package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func validUtf8(data []int) bool {
	var i int = 0
	var n int = len(data)
	for (i < n) {
		var b int = data[i]
		var count int = 0
		if (b < 128) {
			count = 1
		} else 		if ((b >= 192) && (b < 224)) {
			count = 2
		} else 		if ((b >= 224) && (b < 240)) {
			count = 3
		} else 		if ((b >= 240) && (b < 248)) {
			count = 4
		} else {
			return false
		}
		if ((i + count) > n) {
			return false
		}
		var j int = 1
		for (j < count) {
			var c int = data[(i + j)]
			if ((c < 128) || (c >= 192)) {
				return false
			}
			j = (j + 1)
		}
		i = (i + count)
	}
	return true
}

func example_1() {
	expect((validUtf8([]int{197, 130, 1}) == true))
}

func example_2() {
	expect((validUtf8([]int{235, 140, 4}) == false))
}

func single_byte() {
	expect((validUtf8([]int{0}) == true))
}

func invalid_length() {
	expect((validUtf8([]int{237}) == false))
}

func starts_with_continuation() {
	expect((validUtf8([]int{145}) == false))
}

func main() {
	example_1()
	example_2()
	single_byte()
	invalid_length()
	starts_with_continuation()
}

