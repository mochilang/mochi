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

func swapPairs(_ nums: [Int]) -> [Int] {
  let nums = nums

  var i = 0
  var result: [Int] = []
  while i < nums.count {
    if i + 1 < nums.count {
      result = result + [_index(nums, i + 1), _index(nums, i)]
    } else {
      result = result + [_index(nums, i)]
    }
    i = i + 2
  }
  return result
}

func test_example_1() {
  expect(swapPairs([1, 2, 3, 4]) == [2, 1, 4, 3])
}

func test_example_2() {
  expect(swapPairs([]) == [])
}

func test_example_3() {
  expect(swapPairs([1]) == [1])
}

func test_odd_length() {
  expect(swapPairs([1, 2, 3]) == [2, 1, 3])
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
  test_odd_length()
}
main()
