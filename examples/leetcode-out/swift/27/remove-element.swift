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

func _slice<T>(_ arr: [T], _ i: Int, _ j: Int) -> [T] {
  var start = i
  var end = j
  let n = arr.count
  if start < 0 { start += n }
  if end < 0 { end += n }
  if start < 0 { start = 0 }
  if end > n { end = n }
  if end < start { end = start }
  return Array(arr[start..<end])
}

func removeElement(_ nums: [Int], _ val: Int) -> Int {
  var nums = nums
  let val = val

  var k = 0
  var i = 0
  while i < nums.count {
    if _index(nums, i) != val {
      nums[k] = _index(nums, i)
      k = k + 1
    }
    i = i + 1
  }
  return k
}

func test_example_1() {
  var nums: [Int] = [3, 2, 2, 3]
  let k = removeElement(nums, 3)
  expect(k == 2)
  expect(_slice(nums, 0, k) == [2, 2])
}

func test_example_2() {
  var nums: [Int] = [0, 1, 2, 2, 3, 0, 4, 2]
  let k = removeElement(nums, 2)
  expect(k == 5)
  expect(_slice(nums, 0, k) == [0, 1, 3, 0, 4])
}

func test_no_removal() {
  var nums: [Int] = [1, 2, 3]
  let k = removeElement(nums, 4)
  expect(k == 3)
  expect(_slice(nums, 0, k) == [1, 2, 3])
}

func main() {
  test_example_1()
  test_example_2()
  test_no_removal()
}
main()
