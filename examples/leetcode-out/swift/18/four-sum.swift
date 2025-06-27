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

func fourSum(_ nums: [Int], _ target: Int) -> [[Int]] {
  let nums = nums
  let target = target

  let sorted: [Int] = nums.sorted()
  let n = sorted.count
  var result: [[Int]] = []
  for i in 0..<n {
    if i > 0 && _index(sorted, i) == _index(sorted, i - 1) {
      continue
    }
    for j in i + 1..<n {
      if j > i + 1 && _index(sorted, j) == _index(sorted, j - 1) {
        continue
      }
      var left = j + 1
      var right = n - 1
      while left < right {
        let sum =
          _index(sorted, i) + _index(sorted, j) + _index(sorted, left) + _index(sorted, right)
        if sum == target {
          result =
            result + [
              [_index(sorted, i), _index(sorted, j), _index(sorted, left), _index(sorted, right)]
            ]
          left = left + 1
          right = right - 1
          while left < right && _index(sorted, left) == _index(sorted, left - 1) {
            left = left + 1
          }
          while left < right && _index(sorted, right) == _index(sorted, right + 1) {
            right = right - 1
          }
        } else if sum < target {
          left = left + 1
        } else {
          right = right - 1
        }
      }
    }
  }
  return result
}

func test_example_1() {
  expect(fourSum([1, 0, -1, 0, -2, 2], 0) == [[-2, -1, 1, 2], [-2, 0, 0, 2], [-1, 0, 0, 1]])
}

func test_example_2() {
  expect(fourSum([2, 2, 2, 2, 2], 8) == [[2, 2, 2, 2]])
}

func main() {
  test_example_1()
  test_example_2()
}
main()
