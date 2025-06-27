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

func mergeKLists(_ lists: [[Int]]) -> [Int] {
  let lists = lists

  let k = lists.count
  var indices: [Int] = []
  var i = 0
  while i < k {
    indices = indices + [0]
    i = i + 1
  }
  var result: [Int] = []
  while true {
    var best = 0
    var bestList = -1
    var found = false
    var j = 0
    while j < k {
      let idx = _index(indices, j)
      if idx < _index(lists, j).count {
        let val = _index(_index(lists, j), idx)
        if !found || val < best {
          best = val
          bestList = j
          found = true
        }
      }
      j = j + 1
    }
    if !found {
      break
    }
    result = result + [best]
    indices[bestList] = _index(indices, bestList) + 1
  }
  return result
}

func test_example_1() {
  expect(mergeKLists([[1, 4, 5], [1, 3, 4], [2, 6]]) == [1, 1, 2, 3, 4, 4, 5, 6])
}

func test_example_2() {
  expect(mergeKLists([]) == [])
}

func test_example_3() {
  expect(mergeKLists([[]]) == [])
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
}
main()
