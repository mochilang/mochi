package main

import (
	"encoding/json"
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func calcEquation(eqs [][]string, vals []float64, queries [][]string) []float64 {
	var graph map[string][][]any = map[string][][]any{}
	var i int = 0
	for (i < len(eqs)) {
		var a string = eqs[i][0]
		var b string = eqs[i][1]
		var v float64 = vals[i]
		var listA [][]any = [][]any{}
		_tmp0 := a
		_tmp1 := graph
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			listA = graph[a]
		}
		listA = append(append([][]any{}, listA...), [][]any{[]any{b, v}}...)
		graph[a] = listA
		var listB [][]any = [][]any{}
		_tmp3 := b
		_tmp4 := graph
		_, _tmp5 := _tmp4[_tmp3]
		if _tmp5 {
			listB = graph[b]
		}
		listB = append(append([][]any{}, listB...), [][]any{[]any{a, (1 / v)}}...)
		graph[b] = listB
		i = (i + 1)
	}
	var results []float64 = []float64{}
	for _, q := range queries {
		var start string = q[0]
		var end string = q[1]
		_tmp6 := start
		_tmp7 := graph
		_, _tmp8 := _tmp7[_tmp6]
		_tmp9 := end
		_tmp10 := graph
		_, _tmp11 := _tmp10[_tmp9]
		if ((!(_tmp8)) || (!(_tmp11))) {
			results = append(append([]float64{}, results...), []float64{-1}...)
			continue
		}
		var visited map[string]bool = map[string]bool{"start": true}
		var queue [][]any = [][]any{[]any{start, 1}}
		var idx int = 0
		var found bool = false
		var ans float64 = -1
		for (idx < len(queue)) {
			var pair []any = queue[idx]
			var node any = pair[0]
			var val float64 = _cast[float64](pair[1])
			if _equal(node, end) {
				ans = val
				found = true
				break
			}
			var neighbors [][]any = graph[node]
			for _, nb := range neighbors {
				var nxt any = nb[0]
				var ratio float64 = _cast[float64](nb[1])
				var seen bool = false
				_tmp12 := nxt
				_tmp13 := visited
				_, _tmp14 := _tmp13[_tmp12]
				if _tmp14 {
					seen = visited[nxt]
				}
				if !seen {
					visited[nxt] = true
					queue = append(append([][]any{}, queue...), [][]any{[]any{nxt, (val * ratio)}}...)
				}
			}
			idx = (idx + 1)
		}
		if found {
			results = append(append([]float64{}, results...), []float64{ans}...)
		} else {
			results = append(append([]float64{}, results...), []float64{-1}...)
		}
	}
	return results
}

func example_1() {
	var eq [][]string = [][]string{[]string{"a", "b"}, []string{"b", "c"}}
	_ = eq
	var val []float64 = []float64{2, 3}
	_ = val
	var queries [][]string = [][]string{[]string{"a", "c"}, []string{"b", "a"}, []string{"a", "e"}, []string{"a", "a"}, []string{"x", "x"}}
	_ = queries
	expect(_equal(calcEquation(eq, val, queries), []float64{6, 0.5, -1, 1, -1}))
}

func disconnected() {
	var eq [][]string = [][]string{[]string{"a", "b"}, []string{"c", "d"}}
	_ = eq
	var val []float64 = []float64{1.5, 2.5}
	_ = val
	var queries [][]string = [][]string{[]string{"a", "d"}, []string{"c", "a"}}
	_ = queries
	expect(_equal(calcEquation(eq, val, queries), []float64{-1, -1}))
}

func main() {
	example_1()
	disconnected()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
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

