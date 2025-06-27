import Foundation

func expect(_ cond: Bool) {
  if !cond { fatalError("expect failed") }
}

func _index<T>(_ arr: [T], _ i: Int) -> T {
  var idx = i
  let n = arr.count
  if idx < 0 { idx += n }
  if idx < 0 || idx >= n { fatalError("index out of range") }
  return arr[idx]
}

func removeNthFromEnd(_ nums: [Int], _ n: Int) -> [Int] {
  let nums = nums
  let n = n

  let idx = nums.count - n
  var result: [Int] = []
  var i = 0
  while i < nums.count {
    if i != idx {
      result = result + [_index(nums, i)]
    }
    i = i + 1
  }
  return result
}

func test_example_1() {
  expect(removeNthFromEnd([1, 2, 3, 4, 5], 2) == [1, 2, 3, 5])
}

func test_example_2() {
  expect(removeNthFromEnd([1], 1) == [])
}

func test_example_3() {
  expect(removeNthFromEnd([1, 2], 1) == [1])
}

func test_remove_first() {
  expect(removeNthFromEnd([7, 8, 9], 3) == [8, 9])
}

func test_remove_last() {
  expect(removeNthFromEnd([7, 8, 9], 1) == [7, 8])
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
  test_remove_first()
  test_remove_last()
}
main()
