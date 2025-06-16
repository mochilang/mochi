package main

import (
	"encoding/json"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func NewStack() map[string]any {
	return _cast[map[string]any](map[string][]int{"q": _cast[[]int]([]any{})})
}

func push(stack map[string]any, x int) {
	var q []int = _cast[[]int](stack["q"])
	q = append(append([]int{}, q...), []int{x}...)
	var i int = 0
	for (i < (len(q) - 1)) {
		q = append(append([]int{}, q[1:len(q)]...), []int{q[0]}...)
		i = (i + 1)
	}
	stack["q"] = q
}

func pop(stack map[string]any) int {
	var q []int = _cast[[]int](stack["q"])
	var v int = q[0]
	q = q[1:len(q)]
	stack["q"] = q
	return v
}

func top(stack map[string]any) int {
	var q []int = _cast[[]int](stack["q"])
	return q[0]
}

func empty(stack map[string]any) bool {
	var q []int = _cast[[]int](stack["q"])
	return (len(q) == 0)
}

func example() {
	var st map[string]any = NewStack()
	push(st, 1)
	push(st, 2)
	expect((top(st) == 2))
	expect((pop(st) == 2))
	expect((empty(st) == false))
}

func single_push_pop() {
	var st map[string]any = NewStack()
	push(st, 5)
	expect((pop(st) == 5))
	expect((empty(st) == true))
}

func multiple_pushes() {
	var st map[string]any = NewStack()
	push(st, 1)
	push(st, 2)
	push(st, 3)
	expect((pop(st) == 3))
	expect((pop(st) == 2))
	expect((pop(st) == 1))
	expect((empty(st) == true))
}

func main() {
	example()
	single_push_pop()
	multiple_pushes()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

