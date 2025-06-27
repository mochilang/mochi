import Foundation

func expect(_ cond: Bool) {
  if !cond { fatalError("expect failed") }
}

func reverse(_ x: Int) -> Int {
  let x = x

  var sign = 1
  var n = x
  if n < 0 {
    sign = -1
    n = -n
  }
  var rev = 0
  while n != 0 {
    let digit = n % 10
    rev = rev * 10 + digit
    n = n / 10
  }
  rev = rev * sign
  if rev < (-2_147_483_647 - 1) || rev > 2_147_483_647 {
    return 0
  }
  return rev
}

func test_example_1() {
  expect(reverse(123) == 321)
}

func test_example_2() {
  expect(reverse(-123) == (-321))
}

func test_example_3() {
  expect(reverse(120) == 21)
}

func test_overflow() {
  expect(reverse(1_534_236_469) == 0)
}

func main() {
  test_example_1()
  test_example_2()
  test_example_3()
  test_overflow()
}
main()
