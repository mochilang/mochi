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

func lengthOfLongestSubstring(_ s: String) -> Int {
  let s = s

  let n = s.count
  var start = 0
  var best = 0
  var i = 0
  while i < n {
    var j = start
    while j < i {
      if _indexString(s, j) == _indexString(s, i) {
        start = j + 1
        break
      }
      j = j + 1
    }
    let length = i - start + 1
    if length > best {
      best = length
    }
    i = i + 1
  }
  return best
}

func test_example_1() {
  expect(lengthOfLongestSubstring("abcabcbb") == 3)
}

func test_example_2() {
  expect(lengthOfLongestSubstring("bbbbb") == 1)
}

func test_example_3() {
  expect(lengthOfLongestSubstring("pwwkew") == 3)
}

func test_empty_string() {
  expect(lengthOfLongestSubstring("") == 0)
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
  test_empty_string()
}
main()
