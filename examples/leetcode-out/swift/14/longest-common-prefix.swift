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

func _indexString(_ s: String, _ i: Int) -> String {
  var idx = i
  let chars = Array(s)
  if idx < 0 { idx += chars.count }
  if idx < 0 || idx >= chars.count { fatalError("index out of range") }
  return String(chars[idx])
}

func _sliceString(_ s: String, _ i: Int, _ j: Int) -> String {
  var start = i
  var end = j
  let chars = Array(s)
  let n = chars.count
  if start < 0 { start += n }
  if end < 0 { end += n }
  if start < 0 { start = 0 }
  if end > n { end = n }
  if end < start { end = start }
  return String(chars[start..<end])
}

func longestCommonPrefix(_ strs: [String]) -> String {
  let strs = strs

  if strs.count == 0 {
    return ""
  }
  var prefix = _index(strs, 0)
  for i in 1..<strs.count {
    var j = 0
    let current = _index(strs, i)
    while j < prefix.count && j < current.count {
      if _indexString(prefix, j) != _indexString(current, j) {
        break
      }
      j = j + 1
    }
    prefix = _sliceString(prefix, 0, j)
    if prefix == "" {
      break
    }
  }
  return prefix
}

func test_example_1() {
  expect(longestCommonPrefix(["flower", "flow", "flight"]) == "fl")
}

func test_example_2() {
  expect(longestCommonPrefix(["dog", "racecar", "car"]) == "")
}

func test_single_string() {
  expect(longestCommonPrefix(["single"]) == "single")
}

func test_no_common_prefix() {
  expect(longestCommonPrefix(["a", "b", "c"]) == "")
}

func main() {
  test_example_1()
  test_example_2()
  test_single_string()
  test_no_common_prefix()
}
main()
