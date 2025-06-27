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

func removeDuplicates(_ nums: [Int]) -> Int {
  let nums = nums

  if nums.count == 0 {
    return 0
  }
  var count = 1
  var prev = _index(nums, 0)
  var i = 1
  while i < nums.count {
    let cur = _index(nums, i)
    if cur != prev {
      count = count + 1
      prev = cur
    }
    i = i + 1
  }
  return count
}

func test_example_1() {
  expect(removeDuplicates([1, 1, 2]) == 2)
}

func test_example_2() {
  expect(removeDuplicates([0, 0, 1, 1, 1, 2, 2, 3, 3, 4]) == 5)
}

func test_empty() {
  expect(removeDuplicates([]) == 0)
}

func main() {
  test_example_1()
  test_example_2()
  test_empty()
}
main()
