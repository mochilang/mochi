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

func threeSumClosest(_ nums: [Int], _ target: Int) -> Int {
  let nums = nums
  let target = target

  let sorted: [Int] = nums.sorted()
  let n = sorted.count
  var best = _index(sorted, 0) + _index(sorted, 1) + _index(sorted, 2)
  for i in 0..<n {
    var left = i + 1
    var right = n - 1
    while left < right {
      let sum = _index(sorted, i) + _index(sorted, left) + _index(sorted, right)
      if sum == target {
        return target
      }
      var diff = 0
      if sum > target {
        diff = sum - target
      } else {
        diff = target - sum
      }
      var bestDiff = 0
      if best > target {
        bestDiff = best - target
      } else {
        bestDiff = target - best
      }
      if diff < bestDiff {
        best = sum
      }
      if sum < target {
        left = left + 1
      } else {
        right = right - 1
      }
    }
  }
  return best
}

func test_example_1() {
  expect(threeSumClosest([-1, 2, 1, -4], 1) == 2)
}

func test_example_2() {
  expect(threeSumClosest([0, 0, 0], 1) == 0)
}

func test_additional() {
  expect(threeSumClosest([1, 1, 1, 0], -100) == 2)
}

func main() {
  test_example_1()
  test_example_2()
  test_additional()
}
main()
