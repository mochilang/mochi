package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func numIslands2(m int, n int, positions [][]int) []int {
	var parent []int = []int{}
	var rank []int = []int{}
	var land []bool = []bool{}
	var i int = 0
	for (i < (m * n)) {
		parent = append(append([]int{}, parent...), []int{i}...)
		rank = append(append([]int{}, rank...), []int{0}...)
		land = append(append([]bool{}, land...), []bool{false}...)
		i = (i + 1)
	}
	var find = func(x int) int {
		var p int = parent[x]
		for (p != parent[p]) {
			parent[p] = parent[parent[p]]
			p = parent[p]
		}
		parent[x] = p
		return p
}
	var unite = func(a int, b int, count int) int {
		var pa int = find(a)
		var pb int = find(b)
		if (pa == pb) {
			return count
		}
		if (rank[pa] < rank[pb]) {
			parent[pa] = pb
		} else 	if (rank[pa] > rank[pb]) {
			parent[pb] = pa
		} else {
			parent[pb] = pa
			rank[pa] = (rank[pa] + 1)
		}
		return (count - 1)
}
	var result []int = []int{}
	var count int = 0
	var idx int = 0
	for (idx < len(positions)) {
		var r int = positions[idx][0]
		var c int = positions[idx][1]
		var id int = ((r * n) + c)
		if !land[id] {
			land[id] = true
			count = (count + 1)
			if (r > 0) {
				var up int = ((((r - 1)) * n) + c)
				if land[up] {
					count = unite(id, up, count)
				}
			}
			if ((r + 1) < m) {
				var down int = ((((r + 1)) * n) + c)
				if land[down] {
					count = unite(id, down, count)
				}
			}
			if (c > 0) {
				var left int = ((r * n) + ((c - 1)))
				if land[left] {
					count = unite(id, left, count)
				}
			}
			if ((c + 1) < n) {
				var right int = ((r * n) + ((c + 1)))
				if land[right] {
					count = unite(id, right, count)
				}
			}
		}
		result = append(append([]int{}, result...), []int{count}...)
		idx = (idx + 1)
	}
	return result
}

func example_1() {
	var m int = 3
	_ = m
	var n int = 3
	_ = n
	var positions [][]int = [][]int{[]int{0, 0}, []int{0, 1}, []int{1, 2}, []int{2, 1}, []int{1, 1}}
	_ = positions
	expect(_equal(numIslands2(m, n, positions), []int{1, 1, 2, 3, 1}))
}

func add_same_cell() {
	var m int = 1
	_ = m
	var n int = 2
	_ = n
	var positions [][]int = [][]int{[]int{0, 0}, []int{0, 1}, []int{0, 1}}
	_ = positions
	expect(_equal(numIslands2(m, n, positions), []int{1, 1, 1}))
}

func single_cell() {
	expect(_equal(numIslands2(1, 1, [][]int{[]int{0, 0}}), []int{1}))
}

func main() {
	example_1()
	add_same_cell()
	single_cell()
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

