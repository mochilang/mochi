import Foundation

func expect(_ cond: Bool) {
  if !cond { fatalError("expect failed") }
}

func letterCombinations(_ digits: String) -> [String] {
  let digits = digits

  if digits.count == 0 {
    return []
  }
  let mapping: [String: [String]] = [
    "2": ["a", "b", "c"], "3": ["d", "e", "f"], "4": ["g", "h", "i"], "5": ["j", "k", "l"],
    "6": ["m", "n", "o"], "7": ["p", "q", "r", "s"], "8": ["t", "u", "v"],
    "9": ["w", "x", "y", "z"],
  ]
  var result: [String] = [""]
  for d_ch in digits {
    let d = String(d_ch)
    if !(mapping[d] != nil) {
      continue
    }
    let letters: [String] = mapping[d]!
    let next: [String] =
      ({
        var _res: [String] = []
        for p in result {
          for ch in letters {
            _res.append(p + ch)
          }
        }
        var _items = _res
        return _items
      }())
    result = next
  }
  return result
}

func test_example_1() {
  expect(letterCombinations("23") == ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"])
}

func test_example_2() {
  expect(letterCombinations("") == [])
}

func test_example_3() {
  expect(letterCombinations("2") == ["a", "b", "c"])
}

func test_single_seven() {
  expect(letterCombinations("7") == ["p", "q", "r", "s"])
}

func test_mix() {
  expect(
    letterCombinations("79") == [
      "pw", "px", "py", "pz", "qw", "qx", "qy", "qz", "rw", "rx", "ry", "rz", "sw", "sx", "sy",
      "sz",
    ])
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
  test_single_seven()
  test_mix()
}
main()
