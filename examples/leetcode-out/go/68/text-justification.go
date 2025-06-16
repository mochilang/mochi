package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func fullJustify(words []string, maxWidth int) []string {
	var result []string = []string{}
	var i int = 0
	for (i < len(words)) {
		var j int = i
		var lineLen int = 0
		for (j < len(words)) {
			if (((lineLen + len(words[j])) + ((j - i))) <= maxWidth) {
				lineLen = (lineLen + len(words[j]))
				j = (j + 1)
			} else {
				break
			}
		}
		var line string = ""
		var numWords int = (j - i)
		if ((j == len(words)) || (numWords == 1)) {
			var k int = i
			for (k < j) {
				line = line + words[k]
				if (k < (j - 1)) {
					line = line + " "
				}
				k = (k + 1)
			}
			for (len(line) < maxWidth) {
				line = line + " "
			}
		} else {
			var totalSpaces int = (maxWidth - lineLen)
			var gaps int = (numWords - 1)
			var spaceEach int = (totalSpaces / gaps)
			var extra int = (totalSpaces % gaps)
			var k int = i
			for (k < j) {
				line = line + words[k]
				if (k < (j - 1)) {
					var s int = 0
					for (s < spaceEach) {
						line = line + " "
						s = (s + 1)
					}
					if (extra > 0) {
						line = line + " "
						extra = (extra - 1)
					}
				}
				k = (k + 1)
			}
		}
		result = append(append([]string{}, result...), []string{line}...)
		i = j
	}
	return result
}

func example_1() {
	var words []string = []string{"This", "is", "an", "example", "of", "text", "justification."}
	_ = words
	expect(_equal(fullJustify(words, 16), []string{"This    is    an", "example  of text", "justification.  "}))
}

func example_2() {
	var words []string = []string{"What", "must", "be", "acknowledgment", "shall", "be"}
	_ = words
	expect(_equal(fullJustify(words, 16), []string{"What   must   be", "acknowledgment  ", "shall be        "}))
}

func example_3() {
	var words []string = []string{"Science", "is", "what", "we", "understand", "well", "enough", "to", "explain", "to", "a", "computer.", "Art", "is", "everything", "else", "we", "do"}
	_ = words
	expect(_equal(fullJustify(words, 20), []string{"Science  is  what we", "understand      well", "enough to explain to", "a  computer.  Art is", "everything  else  we", "do                  "}))
}

func main() {
	example_1()
	example_2()
	example_3()
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

