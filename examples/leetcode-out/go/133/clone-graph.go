package main

type Node interface { isNode() }
type Nil struct {
}
func (Nil) isNode() {}
type N struct {
	Val int `json:"val"`
	Neighbors []Node `json:"neighbors"`
}
func (N) isNode() {}

func cloneGraph(root Node) Node {
	var seen map[int]Node = map[int]Node{}
	var dfs func(Node) Node
	dfs = func(n Node) Node {
		return func() any {
		_t := n
		if _, ok := _t.(Nil); ok {
			return Nil{}
		}
		if _tmp0, ok := _t.(N); ok {
			v := _tmp0.Val
			neigh := _tmp0.Neighbors
			return func() {
			_tmp0 := v
			_tmp1 := seen
			_, _tmp2 := _tmp1[_tmp0]
			if _tmp2 {
				return seen[v]
			}
			seen[v] = N{Val: v, Neighbors: []any{}}
			var resultNeighbors []Node = []Node{}
			for _, nb := range neigh {
				resultNeighbors = append(append([]Node{}, resultNeighbors...), []Node{dfs(nb)}...)
			}
			var nodeCopy N = N{Val: v, Neighbors: resultNeighbors}
			seen[v] = nodeCopy
			nodeCopy
	}
		}
		return nil
	}()
}
	return dfs(root)
}

func clone_chain() {
	var c Node = cloneGraph(g1)
	func() func() any {
	_t := c
	if _tmp0, ok := _t.(N); ok {
		v1 := _tmp0.Val
		neigh1 := _tmp0.Neighbors
		return func() {
		expect((v1 == 1))
		var n2 Node = neigh1[0]
		func() func() any {
		_t := n2
		if _tmp0, ok := _t.(N); ok {
			v2 := _tmp0.Val
			neigh2 := _tmp0.Neighbors
			return func() {
			expect((v2 == 2))
			var n3 Node = neigh2[0]
			func() func() any {
			_t := n3
			if _tmp0, ok := _t.(N); ok {
				v3 := _tmp0.Val
				neigh3 := _tmp0.Neighbors
				return func() {
				expect((v3 == 3))
				var n4 Node = neigh3[0]
				func() func() any {
				_t := n4
				if _tmp0, ok := _t.(N); ok {
					v4 := _tmp0.Val
					return func() {
					expect((v4 == 4))
			}
				}
				return func() {
					expect(false)
			}
			}()
		}
			}
			return func() {
				expect(false)
		}
		}()
	}
		}
		return func() {
			expect(false)
	}
	}()
}
	}
	return func() {
		expect(false)
}
}()
}

func independent_copy() {
	var c Node = cloneGraph(g1)
	func() func() any {
	_t := c
	if _tmp1, ok := _t.(N); ok {
		neigh := _tmp1.Neighbors
		return func() {
		func() func() any {
		_t := neigh[0]
		if _tmp0, ok := _t.(N); ok {
			v := _tmp0.Val
			return func() {
			expect((v == 2))
	}
		}
		return func() {
			expect(false)
	}
	}()
}
	}
	return func() {
		expect(false)
}
}()
}

var g4 N = N{Val: 4, Neighbors: []any{}}
var g3 N = N{Val: 3, Neighbors: []N{g4}}
var g2 N = N{Val: 2, Neighbors: []N{g3}}
var g1 N = N{Val: 1, Neighbors: []N{g2}}
func main() {
	clone_chain()
	independent_copy()
}

