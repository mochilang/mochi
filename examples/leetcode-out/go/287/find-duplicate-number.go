package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func findDuplicate(nums []int) int {
	var slow int = nums[0]
	var fast int = nums[0]
	for {
		slow = nums[slow]
		fast = nums[nums[fast]]
		if (slow == fast) {
			break
		}
	}
	var ptr1 int = nums[0]
	var ptr2 int = slow
	for (ptr1 != ptr2) {
		ptr1 = nums[ptr1]
		ptr2 = nums[ptr2]
	}
	return ptr1
}

func example_1() {
	expect((findDuplicate([]int{1, 3, 4, 2, 2}) == 2))
}

func example_2() {
	expect((findDuplicate([]int{3, 1, 3, 4, 2}) == 3))
}

func duplicate_at_end() {
	expect((findDuplicate([]int{1, 4, 6, 2, 6, 3, 5}) == 6))
}

func many_duplicates() {
	expect((findDuplicate([]int{2, 2, 2, 2, 2}) == 2))
}

func main() {
	example_1()
	example_2()
	duplicate_at_end()
	many_duplicates()
}

