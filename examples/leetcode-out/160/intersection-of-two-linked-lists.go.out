package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func getIntersectionNode(next []int, headA int, headB int) int {
	var a int = headA
	var b int = headB
	for (a != b) {
		if (a == (-1)) {
			a = headB
		} else {
			a = next[a]
		}
		if (b == (-1)) {
			b = headA
		} else {
			b = next[b]
		}
	}
	return a
}

func example_1() {
	var next []int = []int{1, 2, 3, 4, -1, 6, 7, 2}
	_ = next
	expect((getIntersectionNode(next, 0, 5) == 2))
}

func example_2() {
	var next []int = []int{1, 2, 3, 4, -1, 3}
	_ = next
	expect((getIntersectionNode(next, 0, 5) == 3))
}

func example_3() {
	var next []int = []int{1, 2, -1, 4, -1}
	_ = next
	expect((getIntersectionNode(next, 0, 3) == (-1)))
}

func same_head() {
	var next []int = []int{1, 2, 3, -1}
	_ = next
	expect((getIntersectionNode(next, 0, 0) == 0))
}

func one_empty() {
	var next []int = []int{1, -1}
	_ = next
	expect((getIntersectionNode(next, 0, -1) == (-1)))
}

func main() {
	example_1()
	example_2()
	example_3()
	same_head()
	one_empty()
}

