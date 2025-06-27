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

func convert(_ s: String, _ numRows: Int) -> String {
  let s = s
  let numRows = numRows

  if numRows <= 1 || numRows >= s.count {
    return s
  }
  var rows: [String] = []
  var i = 0
  while i < numRows {
    rows = rows + [""]
    i = i + 1
  }
  var curr = 0
  var step = 1
  for ch_ch in s {
    let ch = String(ch_ch)
    rows[curr] = String(_index(rows, curr)) + ch
    if curr == 0 {
      step = 1
    } else if curr == numRows - 1 {
      step = -1
    }
    curr = curr + step
  }
  var result: String = ""
  for row in rows {
    result = result + row
  }
  return result
}

func test_example_1() {
  expect(convert("PAYPALISHIRING", 3) == "PAHNAPLSIIGYIR")
}

func test_example_2() {
  expect(convert("PAYPALISHIRING", 4) == "PINALSIGYAHRPI")
}

func test_single_row() {
  expect(convert("A", 1) == "A")
}

func main() {
  test_example_1()
  test_example_2()
  test_single_row()
}
main()
