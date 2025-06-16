package main

import (
	"encoding/json"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func isPalindromeList(values []int) bool {
	var n int = len(values)
	var i int = 0
	var j int = (n - 1)
	for (i < j) {
		if (values[i] != values[j]) {
			return false
		}
		i = (i + 1)
		j = (j - 1)
	}
	return true
}

func example_1() {
	expect((isPalindromeList([]int{1, 2, 2, 1}) == true))
}

func example_2() {
	expect((isPalindromeList([]int{1, 2}) == false))
}

func single_element() {
	expect((isPalindromeList([]int{7}) == true))
}

func empty_list() {
	expect((isPalindromeList([]int{}) == true))
}

func main() {
	example_1()
	example_2()
	single_element()
	empty_list()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

