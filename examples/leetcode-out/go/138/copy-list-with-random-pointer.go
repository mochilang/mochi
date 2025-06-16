package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type Node struct {
	Val int `json:"val"`
	Next int `json:"next"`
	Random int `json:"random"`
}

func copyRandomList(nodes []Node) []Node {
	var result []Node = []Node{}
	for _, n := range nodes {
		result = append(append([]Node{}, result...), []Node{Node{Val: n.Val, Next: n.Next, Random: n.Random}}...)
	}
	return result
}

func serialize(nodes []Node) [][]int {
	var out [][]int = [][]int{}
	var i int = 0
	for (i < len(nodes)) {
		var n Node = nodes[i]
		_ = n
		out = append(append([][]int{}, out...), [][]int{[]int{n.Val, n.Random}}...)
		i = (i + 1)
	}
	return out
}

func copy_list() {
	var original []Node = []Node{Node{Val: 7, Next: 1, Random: -1}, Node{Val: 13, Next: 2, Random: 0}, Node{Val: 11, Next: 3, Random: 4}, Node{Val: 10, Next: 4, Random: 2}, Node{Val: 1, Next: -1, Random: 0}}
	var copied []Node = copyRandomList(original)
	_ = copied
	expect(_equal(serialize(copied), serialize(original)))
}

func main() {
	copy_list()
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

