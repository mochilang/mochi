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

func threeSum(_ nums: [Int]) -> [[Int]] {
  let nums = nums

  let sorted: [Int] = nums.sorted()
  let n = sorted.count
  var res: [[Int]] = []
  var i = 0
  while i < n {
    if i > 0 && _index(sorted, i) == _index(sorted, i - 1) {
      i = i + 1
      continue
    }
    var left = i + 1
    var right = n - 1
    while left < right {
      let sum = _index(sorted, i) + _index(sorted, left) + _index(sorted, right)
      if sum == 0 {
        res = res + [[_index(sorted, i), _index(sorted, left), _index(sorted, right)]]
        left = left + 1
        while left < right && _index(sorted, left) == _index(sorted, left - 1) {
          left = left + 1
        }
        right = right - 1
        while left < right && _index(sorted, right) == _index(sorted, right + 1) {
          right = right - 1
        }
      } else if sum < 0 {
        left = left + 1
      } else {
        right = right - 1
      }
    }
    i = i + 1
  }
  return res
}

func test_example_1() {
  expect(threeSum([-1, 0, 1, 2, -1, -4]) == [[-1, -1, 2], [-1, 0, 1]])
}

func test_example_2() {
  expect(threeSum([0, 1, 1]) == [])
}

func test_example_3() {
  expect(threeSum([0, 0, 0]) == [[0, 0, 0]])
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
}
main()
