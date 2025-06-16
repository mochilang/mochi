package main

import (
	"encoding/json"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type MyQueue struct {
	Ins []int `json:"ins"`
	Outs []int `json:"outs"`
}

type PopResult struct {
	Queue MyQueue `json:"queue"`
	Val int `json:"val"`
}

func newQueue() MyQueue {
	return MyQueue{Ins: _cast[[]int]([]any{}), Outs: _cast[[]int]([]any{})}
}

func push(q MyQueue, x int) MyQueue {
	var s []int = q.Ins
	s = append(append([]int{}, s...), []int{x}...)
	return MyQueue{Ins: s, Outs: q.Outs}
}

func ensureOut(q MyQueue) MyQueue {
	var inStack []int = q.Ins
	var outStack []int = q.Outs
	if (len(outStack) == 0) {
		for (len(inStack) > 0) {
			var v int = inStack[(len(inStack) - 1)]
			inStack = inStack[0:(len(inStack) - 1)]
			outStack = append(append([]int{}, outStack...), []int{v}...)
		}
	}
	return MyQueue{Ins: inStack, Outs: outStack}
}

func pop(q MyQueue) PopResult {
	var shifted MyQueue = ensureOut(q)
	_ = shifted
	var outStack []int = shifted.Outs
	var v int = outStack[(len(outStack) - 1)]
	outStack = outStack[0:(len(outStack) - 1)]
	var newQ MyQueue = MyQueue{Ins: shifted.Ins, Outs: outStack}
	return PopResult{Queue: newQ, Val: v}
}

func peek(q MyQueue) int {
	var shifted MyQueue = ensureOut(q)
	_ = shifted
	return shifted.Outs[(len(shifted.Outs) - 1)]
}

func empty(q MyQueue) bool {
	return ((len(q.Ins) == 0) && (len(q.Outs) == 0))
}

func example() {
	var q MyQueue = newQueue()
	q = push(q, 1)
	q = push(q, 2)
	expect((peek(q) == 1))
	var r1 PopResult = pop(q)
	_ = r1
	q = r1.Queue
	expect((r1.Val == 1))
	expect((empty(q) == false))
}

func multiple_operations() {
	var q MyQueue = newQueue()
	q = push(q, 3)
	q = push(q, 4)
	var r1 PopResult = pop(q)
	_ = r1
	q = r1.Queue
	expect((r1.Val == 3))
	q = push(q, 5)
	expect((peek(q) == 4))
	var r2 PopResult = pop(q)
	_ = r2
	q = r2.Queue
	expect((r2.Val == 4))
	var r3 PopResult = pop(q)
	_ = r3
	q = r3.Queue
	expect((r3.Val == 5))
	expect((empty(q) == true))
}

func main() {
	example()
	multiple_operations()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

