package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func countComponents(n int, edges [][]int) int {
	var graph [][]int = [][]int{}
	var i int = 0
	for (i < n) {
		graph = append(append([]any{}, _toAnySlice(graph)...), _toAnySlice([][]any{[]any{}})...)
		i = (i + 1)
	}
	for _, e := range edges {
		var u int = e[0]
		var v int = e[1]
		graph[u] = append(append([]int{}, graph[u]...), []int{v}...)
		graph[v] = append(append([]int{}, graph[v]...), []int{u}...)
	}
	var visited []bool = []bool{}
	i = 0
	for (i < n) {
		visited = append(append([]bool{}, visited...), []bool{false}...)
		i = (i + 1)
	}
	var components int = 0
	var start int = 0
	for (start < n) {
		if !visited[start] {
			components = (components + 1)
			var queue []int = []int{start}
			visited[start] = true
			var idx int = 0
			for (idx < len(queue)) {
				var cur int = queue[idx]
				idx = (idx + 1)
				for _, nei := range graph[cur] {
					if !visited[nei] {
						visited[nei] = true
						queue = append(append([]int{}, queue...), []int{nei}...)
					}
				}
			}
		}
		start = (start + 1)
	}
	return components
}

func example_1() {
	expect((countComponents(5, [][]int{[]int{0, 1}, []int{1, 2}, []int{3, 4}}) == 2))
}

func example_2() {
	expect((countComponents(5, [][]int{[]int{0, 1}, []int{1, 2}, []int{2, 3}, []int{3, 4}}) == 1))
}

func single_node() {
	expect((countComponents(1, [][]int{}) == 1))
}

func two_disjoint_edges() {
	expect((countComponents(4, [][]int{[]int{0, 1}, []int{2, 3}}) == 2))
}

func no_edges() {
	expect((countComponents(4, [][]int{}) == 4))
}

func main() {
	example_1()
	example_2()
	single_node()
	two_disjoint_edges()
	no_edges()
}

func _toAnySlice[T any](s []T) []any {
    out := make([]any, len(s))
    for i, v := range s { out[i] = v }
    return out
}

