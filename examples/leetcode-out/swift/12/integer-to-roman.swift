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

func intToRoman(_ num: Int) -> String {
  var num = num

  let values: [Int] = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
  let symbols: [String] = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
  var result = ""
  var i = 0
  while num > 0 {
    while num >= _index(values, i) {
      result = result + String(_index(symbols, i))
      num = num - _index(values, i)
    }
    i = i + 1
  }
  return result
}

func test_example_1() {
  expect(intToRoman(3) == "III")
}

func test_example_2() {
  expect(intToRoman(58) == "LVIII")
}

func test_example_3() {
  expect(intToRoman(1994) == "MCMXCIV")
}

func test_small_numbers() {
  expect(intToRoman(4) == "IV")
  expect(intToRoman(9) == "IX")
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
  test_small_numbers()
}
main()
