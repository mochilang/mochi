package main

import (
	"encoding/json"
	"fmt"
)

type Tree interface { isTree() }
type Empty struct {
}
func (Empty) isTree() {}
type Node struct {
	Left Tree `json:"left"`
	Val int `json:"val"`
	Right Tree `json:"right"`
}
func (Node) isTree() {}

func build(start int, end int) []Tree {
	if (start > end) {
		return _cast[[]Tree]([]Empty{Empty{}})
	}
	var result []Tree = []Tree{}
	for i := start; i < (end + 1); i++ {
		var leftTrees []Tree = build(start, (i - 1))
		var rightTrees []Tree = build((i + 1), end)
		for _, l := range leftTrees {
			for _, r := range rightTrees {
				result = append(append([]Tree{}, result...), []Node{Node{Left: l, Val: i, Right: r}}...)
			}
		}
	}
	return result
}

func generateTrees(n int) []Tree {
	if (n == 0) {
		return _cast[[]Tree]([]any{})
	}
	return build(1, n)
}

func main() {
	var trees []Tree = generateTrees(3)
	fmt.Println(len(trees))
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

