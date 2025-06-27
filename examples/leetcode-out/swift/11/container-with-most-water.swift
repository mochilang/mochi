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

func maxArea(_ height: [Int]) -> Int {
  let height = height

  var left = 0
  var right = height.count - 1
  var maxArea = 0
  while left < right {
    let width = right - left
    var h = 0
    if _index(height, left) < _index(height, right) {
      h = _index(height, left)
    } else {
      h = _index(height, right)
    }
    let area = h * width
    if area > maxArea {
      maxArea = area
    }
    if _index(height, left) < _index(height, right) {
      left = left + 1
    } else {
      right = right - 1
    }
  }
  return maxArea
}

func test_example_1() {
  expect(maxArea([1, 8, 6, 2, 5, 4, 8, 3, 7]) == 49)
}

func test_example_2() {
  expect(maxArea([1, 1]) == 1)
}

func test_decreasing_heights() {
  expect(maxArea([4, 3, 2, 1, 4]) == 16)
}

func test_short_array() {
  expect(maxArea([1, 2, 1]) == 2)
}

func main() {
  test_example_1()
  test_example_2()
  test_decreasing_heights()
  test_short_array()
}
main()
