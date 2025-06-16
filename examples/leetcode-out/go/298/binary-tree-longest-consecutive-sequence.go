package main

import (
	"encoding/json"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func longestConsecutive(lefts []int, rights []int, values []int, root int) int {
	if (root == (-1)) {
		return 0
	}
	var dfs func(int, int, int) int
	dfs = func(node int, parentVal int, length int) int {
		if (node == (-1)) {
			return length
		}
		var curr int = 1
		if (values[node] == (parentVal + 1)) {
			curr = (length + 1)
		}
		var leftLen int = dfs(lefts[node], values[node], curr)
		var rightLen int = dfs(rights[node], values[node], curr)
		var best int = curr
		if (leftLen > best) {
			best = leftLen
		}
		if (rightLen > best) {
			best = rightLen
		}
		return best
}
	return dfs(root, (values[root] - 1), 0)
}

func simple_chain() {
	expect((longestConsecutive(lefts1, rights1, values1, root1) == 2))
}

func increasing_right() {
	expect((longestConsecutive(lefts2, rights2, values2, root2) == 4))
}

func mixed_values() {
	expect((longestConsecutive(lefts3, rights3, values3, root3) == 2))
}

var lefts1 []int = []int{1, (-1), (-1)}
var rights1 []int = []int{2, (-1), (-1)}
var values1 []int = []int{1, 2, 3}
var root1 int = 0
var lefts2 []int = []int{(-1), 2, (-1), (-1), (-1), (-1)}
var rights2 []int = []int{1, 4, 3, 5, (-1), (-1)}
var values2 []int = []int{1, 2, 3, 4, 5, 6}
var root2 int = 0
var lefts3 []int = []int{1, 3, (-1), (-1)}
var rights3 []int = []int{2, (-1), (-1), (-1)}
var values3 []int = []int{3, 2, 4, 1}
var root3 int = 0
func main() {
	simple_chain()
	increasing_right()
	mixed_values()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

