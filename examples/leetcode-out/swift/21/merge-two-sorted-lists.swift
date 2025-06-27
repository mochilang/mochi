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

func mergeTwoLists(_ l1: [Int], _ l2: [Int]) -> [Int] {
  let l1 = l1
  let l2 = l2

  var i = 0
  var j = 0
  var result: [Int] = []
  while i < l1.count && j < l2.count {
    if _index(l1, i) <= _index(l2, j) {
      result = result + [_index(l1, i)]
      i = i + 1
    } else {
      result = result + [_index(l2, j)]
      j = j + 1
    }
  }
  while i < l1.count {
    result = result + [_index(l1, i)]
    i = i + 1
  }
  while j < l2.count {
    result = result + [_index(l2, j)]
    j = j + 1
  }
  return result
}

func test_example_1() {
  expect(mergeTwoLists([1, 2, 4], [1, 3, 4]) == [1, 1, 2, 3, 4, 4])
}

func test_example_2() {
  expect(mergeTwoLists([], []) == [])
}

func test_example_3() {
  expect(mergeTwoLists([], [0]) == [0])
}

func test_different_lengths() {
  expect(mergeTwoLists([1, 5, 7], [2, 3, 4, 6, 8]) == [1, 2, 3, 4, 5, 6, 7, 8])
}

func test_one_list_empty() {
  expect(mergeTwoLists([1, 2, 3], []) == [1, 2, 3])
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
  test_different_lengths()
  test_one_list_empty()
}
main()
