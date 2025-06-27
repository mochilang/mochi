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

func reverseKGroup(_ nums: [Int], _ k: Int) -> [Int] {
  let nums = nums
  let k = k

  let n = nums.count
  if k <= 1 {
    return nums
  }
  var result: [Int] = []
  var i = 0
  while i < n {
    let end = i + k
    if end <= n {
      var j = end - 1
      while j >= i {
        result = result + [_index(nums, j)]
        j = j - 1
      }
    } else {
      var j = i
      while j < n {
        result = result + [_index(nums, j)]
        j = j + 1
      }
    }
    i = i + k
  }
  return result
}

func test_example_1() {
  expect(reverseKGroup([1, 2, 3, 4, 5], 2) == [2, 1, 4, 3, 5])
}

func test_example_2() {
  expect(reverseKGroup([1, 2, 3, 4, 5], 3) == [3, 2, 1, 4, 5])
}

func test_k_equals_list_length() {
  expect(reverseKGroup([1, 2, 3, 4], 4) == [4, 3, 2, 1])
}

func test_k_greater_than_length() {
  expect(reverseKGroup([1, 2, 3], 5) == [1, 2, 3])
}

func test_k_is_one() {
  expect(reverseKGroup([1, 2, 3], 1) == [1, 2, 3])
}

func main() {
  test_example_1()
  test_example_2()
  test_k_equals_list_length()
  test_k_greater_than_length()
  test_k_is_one()
}
main()
