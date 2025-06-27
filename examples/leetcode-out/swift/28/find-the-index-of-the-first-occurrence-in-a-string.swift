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

func strStr(_ haystack: String, _ needle: String) -> Int {
  let haystack = haystack
  let needle = needle

  let n = haystack.count
  let m = needle.count
  if m == 0 {
    return 0
  }
  if m > n {
    return -1
  }
  for i in 0..<n - m + 1 {
    var j = 0
    while j < m {
      if _indexString(haystack, i + j) != _indexString(needle, j) {
        break
      }
      j = j + 1
    }
    if j == m {
      return i
    }
  }
  return -1
}

func test_example_1() {
  expect(strStr("sadbutsad", "sad") == 0)
}

func test_example_2() {
  expect(strStr("leetcode", "leeto") == (-1))
}

func test_empty_needle() {
  expect(strStr("abc", "") == 0)
}

func test_needle_at_end() {
  expect(strStr("hello", "lo") == 3)
}

func main() {
  test_example_1()
  test_example_2()
  test_empty_needle()
  test_needle_at_end()
}
main()
