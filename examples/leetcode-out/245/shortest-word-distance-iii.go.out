package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func abs(x int) int {
	if (x < 0) {
		return -x
	} else {
		return x
	}
}

func shortestWordDistance(words []string, word1 string, word2 string) int {
	var index1 int = (-1)
	var index2 int = (-1)
	var minDist int = len(words)
	var i int = 0
	for (i < len(words)) {
		var w string = words[i]
		if (w == word1) {
			if (word1 == word2) {
				if (index1 != (-1)) {
					var d int = (i - index1)
					if (d < minDist) {
						minDist = d
					}
				}
				index1 = i
			} else {
				index1 = i
				if (index2 != (-1)) {
					var d int = abs((index1 - index2))
					if (d < minDist) {
						minDist = d
					}
				}
			}
		} else 		if (w == word2) {
			index2 = i
			if (index1 != (-1)) {
				var d int = abs((index1 - index2))
				if (d < minDist) {
					minDist = d
				}
			}
		}
		i = (i + 1)
	}
	return minDist
}

func example_1() {
	expect((shortestWordDistance([]string{"practice", "makes", "perfect", "coding", "makes"}, "makes", "coding") == 1))
}

func example_2() {
	expect((shortestWordDistance([]string{"practice", "makes", "perfect", "coding", "makes"}, "makes", "makes") == 3))
}

func example_3() {
	expect((shortestWordDistance([]string{"a", "a", "b", "b"}, "a", "b") == 1))
}

func main() {
	example_1()
	example_2()
	example_3()
}

