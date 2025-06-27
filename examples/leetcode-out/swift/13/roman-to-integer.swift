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

func romanToInt(_ s: String) -> Int {
  let s = s

  let values: [String: Int] = ["I": 1, "V": 5, "X": 10, "L": 50, "C": 100, "D": 500, "M": 1000]
  var total = 0
  var i = 0
  let n = s.count
  while i < n {
    let curr = values[_indexString(s, i)]!
    if i + 1 < n {
      let next = values[_indexString(s, i + 1)]!
      if curr < next {
        total = total + next - curr
        i = i + 2
        continue
      }
    }
    total = total + curr
    i = i + 1
  }
  return total
}

func test_example_1() {
  expect(romanToInt("III") == 3)
}

func test_example_2() {
  expect(romanToInt("LVIII") == 58)
}

func test_example_3() {
  expect(romanToInt("MCMXCIV") == 1994)
}

func test_subtractive() {
  expect(romanToInt("IV") == 4)
  expect(romanToInt("IX") == 9)
}

func test_tens() {
  expect(romanToInt("XL") == 40)
  expect(romanToInt("XC") == 90)
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
  test_subtractive()
  test_tens()
}
main()
