package vm

import (
	"sort"
	"testing"
)

func makeRows(n int) []Value {
	rows := make([]Value, n)
	for i := 0; i < n; i++ {
		rows[i] = Value{Tag: ValueMap, Map: map[string]Value{"id": {Tag: ValueInt, Int: i}}}
	}
	return rows
}

func BenchmarkNestedLoopJoin(b *testing.B) {
	left := makeRows(100)
	right := makeRows(100)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = NestedLoopJoin(left, right)
	}
}

func BenchmarkHashJoin(b *testing.B) {
	left := makeRows(100)
	right := makeRows(100)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = HashJoin(left, right)
	}
}

func BenchmarkHashJoinPrealloc(b *testing.B) {
	left := makeRows(100)
	right := makeRows(100)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = HashJoinPrealloc(left, right)
	}
}

func BenchmarkMergeJoin(b *testing.B) {
	left := makeRows(100)
	right := makeRows(100)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = MergeJoin(left, right)
	}
}

func BenchmarkIndexJoin(b *testing.B) {
	left := makeRows(100)
	right := makeRows(100)
	sort.Slice(right, func(i, j int) bool {
		return right[i].Map["id"].Int < right[j].Map["id"].Int
	})
	ids := make([]int, len(right))
	for i, r := range right {
		ids[i] = r.Map["id"].Int
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = IndexJoin(left, right, ids)
	}
}
