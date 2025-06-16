package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func validTree(n int, edges [][]int) bool {
	if (len(edges) != (n - 1)) {
		return false
	}
	var parent []int = []int{}
	for i := 0; i < n; i++ {
		parent = append(append([]int{}, parent...), []int{i}...)
	}
	var find = func(x int) int {
		var root int = x
		for (parent[root] != root) {
			root = parent[root]
		}
		var node int = x
		for (parent[node] != node) {
			var next int = parent[node]
			parent[node] = root
			node = next
		}
		return root
}
	for _, e := range edges {
		var a int = e[0]
		var b int = e[1]
		var pa int = find(a)
		var pb int = find(b)
		if (pa == pb) {
			return false
		}
		parent[pb] = pa
	}
	return true
}

func example_1() {
	expect((validTree(5, [][]int{[]int{0, 1}, []int{0, 2}, []int{0, 3}, []int{1, 4}}) == true))
}

func example_2() {
	expect((validTree(5, [][]int{[]int{0, 1}, []int{1, 2}, []int{2, 3}, []int{1, 3}, []int{1, 4}}) == false))
}

func disconnected() {
	expect((validTree(4, [][]int{[]int{0, 1}, []int{2, 3}}) == false))
}

func main() {
	example_1()
	example_2()
	disconnected()
}

