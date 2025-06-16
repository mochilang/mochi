package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type NumArray struct {
	Nums []int `json:"nums"`
}

func newNumArray(values []int) NumArray {
	return NumArray{Nums: values}
}

func update(arr NumArray, index int, val int) NumArray {
	var data []int = arr.Nums
	data[index] = val
	return NumArray{Nums: data}
}

func sumRange(arr NumArray, left int, right int) int {
	var i int = left
	var total int = 0
	for (i <= right) {
		total = (total + arr.Nums[i])
		i = (i + 1)
	}
	return total
}

func example() {
	var na NumArray = newNumArray([]int{1, 3, 5})
	expect((sumRange(na, 0, 2) == 9))
	na = update(na, 1, 2)
	expect((sumRange(na, 0, 2) == 8))
}

func update_first_and_last() {
	var na NumArray = newNumArray([]int{2, 4, 6, 8})
	na = update(na, 0, 1)
	na = update(na, 3, 5)
	expect((sumRange(na, 0, 3) == 16))
	expect((sumRange(na, 1, 2) == 10))
}

func main() {
	example()
	update_first_and_last()
}

