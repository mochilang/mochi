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

func findSubstring(_ s: String, _ words: [String]) -> [Int] {
  let s = s
  let words = words

  if words.count == 0 {
    return []
  }
  let wordLen = _index(words, 0).count
  let wordCount = words.count
  let totalLen = wordLen * wordCount
  if s.count < totalLen {
    return []
  }
  var freq: [String: Int] = [:]
  for w in words {
    if freq[w] != nil {
      freq[w] = freq[w]! + 1
    } else {
      freq[w] = 1
    }
  }
  var result: [Int] = []
  for offset in 0..<wordLen {
    var left = offset
    var count = 0
    var seen: [String: Int] = [:]
    var j = offset
    while j + wordLen <= s.count {
      let word = _sliceString(s, j, j + wordLen)
      j = j + wordLen
      if freq[word] != nil {
        if seen[word] != nil {
          seen[word] = seen[word]! + 1
        } else {
          seen[word] = 1
        }
        count = count + 1
        while seen[word]! > freq[word]! {
          let lw = _sliceString(s, left, left + wordLen)
          seen[lw] = seen[lw]! - 1
          left = left + wordLen
          count = count - 1
        }
        if count == wordCount {
          result = result + [left]
          let lw = _sliceString(s, left, left + wordLen)
          seen[lw] = seen[lw]! - 1
          left = left + wordLen
          count = count - 1
        }
      } else {
        seen = [:]
        count = 0
        left = j
      }
    }
  }
  return result
}

func test_example_1() {
  expect(findSubstring("barfoothefoobarman", ["foo", "bar"]) == [0, 9])
}

func test_example_2() {
  expect(findSubstring("wordgoodgoodgoodbestword", ["word", "good", "best", "word"]) == [])
}

func test_example_3() {
  expect(findSubstring("barfoofoobarthefoobarman", ["bar", "foo", "the"]) == [6, 9, 12])
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
}
main()
