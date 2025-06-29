package vm

// Simple join algorithms used for benchmarking.

// NestedLoopJoin performs a basic nested loop join on the "id" field.
func NestedLoopJoin(left, right []Value) []Value {
	out := make([]Value, 0)
	for _, l := range left {
		lid := l.Map["id"].Int
		for _, r := range right {
			if lid == r.Map["id"].Int {
				out = append(out, Value{Tag: ValueList, List: []Value{l, r}})
			}
		}
	}
	return out
}

// HashJoin performs an inner hash join on the "id" field.
func HashJoin(left, right []Value) []Value {
	buckets := make(map[int][]Value)
	for _, r := range right {
		id := r.Map["id"].Int
		buckets[id] = append(buckets[id], r)
	}
	out := make([]Value, 0)
	for _, l := range left {
		id := l.Map["id"].Int
		if matches, ok := buckets[id]; ok {
			for _, r := range matches {
				out = append(out, Value{Tag: ValueList, List: []Value{l, r}})
			}
		}
	}
	return out
}

// HashJoinPrealloc is like HashJoin but preallocates buckets and the result slice.
func HashJoinPrealloc(left, right []Value) []Value {
	buckets := make(map[int][]Value, len(right))
	for _, r := range right {
		id := r.Map["id"].Int
		buckets[id] = append(buckets[id], r)
	}
	out := make([]Value, 0, len(left))
	for _, l := range left {
		id := l.Map["id"].Int
		if matches, ok := buckets[id]; ok {
			for _, r := range matches {
				out = append(out, Value{Tag: ValueList, List: []Value{l, r}})
			}
		}
	}
	return out
}

// MergeJoin performs an inner merge join on sorted inputs.
func MergeJoin(left, right []Value) []Value {
	out := make([]Value, 0)
	i, j := 0, 0
	for i < len(left) && j < len(right) {
		lid := left[i].Map["id"].Int
		rid := right[j].Map["id"].Int
		if lid == rid {
			out = append(out, Value{Tag: ValueList, List: []Value{left[i], right[j]}})
			i++
			j++
		} else if lid < rid {
			i++
		} else {
			j++
		}
	}
	return out
}
