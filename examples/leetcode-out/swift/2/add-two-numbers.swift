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

func addTwoNumbers(_ l1: [Int], _ l2: [Int]) -> [Int] {
  let l1 = l1
  let l2 = l2

  var i = 0
  var j = 0
  var carry = 0
  var result: [Int] = []
  while i < l1.count || j < l2.count || carry > 0 {
    var x = 0
    if i < l1.count {
      x = _index(l1, i)
      i = i + 1
    }
    var y = 0
    if j < l2.count {
      y = _index(l2, j)
      j = j + 1
    }
    let sum = x + y + carry
    let digit = sum % 10
    carry = sum / 10
    result = result + [digit]
  }
  return result
}

func test_example_1() {
  expect(addTwoNumbers([2, 4, 3], [5, 6, 4]) == [7, 0, 8])
}

func test_example_2() {
  expect(addTwoNumbers([0], [0]) == [0])
}

func test_example_3() {
  expect(addTwoNumbers([9, 9, 9, 9, 9, 9, 9], [9, 9, 9, 9]) == [8, 9, 9, 9, 0, 0, 0, 1])
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
}
main()
