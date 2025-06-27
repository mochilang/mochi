import Foundation

func expect(_ cond: Bool) {
  if !cond { fatalError("expect failed") }
}

func _indexString(_ s: String, _ i: Int) -> String {
  var idx = i
  let chars = Array(s)
  if idx < 0 { idx += chars.count }
  if idx < 0 || idx >= chars.count { fatalError("index out of range") }
  return String(chars[idx])
}

func isPalindrome(_ x: Int) -> Bool {
  let x = x

  if x < 0 {
    return false
  }
  let s: String = String(x)
  let n = s.count
  for i in 0..<n / 2 {
    if _indexString(s, i) != _indexString(s, n - 1 - i) {
      return false
    }
  }
  return true
}

func test_example_1() {
  expect(isPalindrome(121) == true)
}

func test_example_2() {
  expect(isPalindrome(-121) == false)
}

func test_example_3() {
  expect(isPalindrome(10) == false)
}

func test_zero() {
  expect(isPalindrome(0) == true)
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
  test_zero()
}
main()
