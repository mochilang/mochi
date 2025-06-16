package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func ladderLength(beginWord string, endWord string, wordList []string) int {
	var dict map[string]bool = map[string]bool{}
	for _, w := range wordList {
		dict[w] = true
	}
	_tmp0 := endWord
	_tmp1 := dict
	_, _tmp2 := _tmp1[_tmp0]
	if !(_tmp2) {
		return 0
	}
	var queue []string = []string{beginWord}
	var visited map[string]bool = map[string]bool{"beginWord": true}
	var level int = 1
	var letters string = "abcdefghijklmnopqrstuvwxyz"
	for (len(queue) > 0) {
		var next []string = []string{}
		for _, word := range queue {
			if (word == endWord) {
				return level
			}
			for i := 0; i < len(word); i++ {
				for j := 0; j < 26; j++ {
					var ch string = _indexString(letters, j)
					if (ch != _indexString(word, i)) {
						var candidate string = string([]rune(word)[0:i]) + ch + string([]rune(word)[(i + 1):len(word)])
						_tmp3 := candidate
						_tmp4 := visited
						_, _tmp5 := _tmp4[_tmp3]
						_tmp6 := candidate
						_tmp7 := dict
						_, _tmp8 := _tmp7[_tmp6]
						if (_tmp8 && ((_tmp5) == false)) {
							visited[candidate] = true
							next = append(append([]string{}, next...), []string{candidate}...)
						}
					}
				}
			}
		}
		queue = next
		level = (level + 1)
	}
	return 0
}

func example_1() {
	expect((ladderLength("hit", "cog", []string{"hot", "dot", "dog", "lot", "log", "cog"}) == 5))
}

func example_2() {
	expect((ladderLength("hit", "cog", []string{"hot", "dot", "dog", "lot", "log"}) == 0))
}

func main() {
	example_1()
	example_2()
}

func _indexString(s string, i int) string {
    runes := []rune(s)
    if i < 0 {
        i += len(runes)
    }
    if i < 0 || i >= len(runes) {
        panic("index out of range")
    }
    return string(runes[i])
}

