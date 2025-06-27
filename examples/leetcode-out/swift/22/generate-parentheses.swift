import Foundation

func expect(_ cond: Bool) {
  if !cond { fatalError("expect failed") }
}

func generateParenthesis(_ n: Int) -> [String] {
  let n = n

  var result: [String] = []
  func backtrack(_ current: String, _ open: Int, _ close: Int) {
    let current = current
    let open = open
    let close = close

    if current.count == n * 2 {
      result = result + [current]
    } else {
      if open < n {
        backtrack(current + "(", open + 1, close)
      }
      if close < open {
        backtrack(current + ")", open, close + 1)
      }
    }
  }
  backtrack("", 0, 0)
  return result
}

func test_example_1() {
  expect(generateParenthesis(3) == ["((()))", "(()())", "(())()", "()(())", "()()()"])
}

func test_example_2() {
  expect(generateParenthesis(1) == ["()"])
}

func test_two_pairs() {
  expect(generateParenthesis(2) == ["(())", "()()"])
}

func main() {
  test_example_1()
  test_example_2()
  test_two_pairs()
}
main()
