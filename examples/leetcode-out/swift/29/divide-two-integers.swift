import Foundation

func expect(_ cond: Bool) {
  if !cond { fatalError("expect failed") }
}

func divide(_ dividend: Int, _ divisor: Int) -> Int {
  var dividend = dividend
  var divisor = divisor

  if dividend == (-2_147_483_647 - 1) && divisor == (-1) {
    return 2_147_483_647
  }
  var negative = false
  if dividend < 0 {
    negative = !negative
    dividend = -dividend
  }
  if divisor < 0 {
    negative = !negative
    divisor = -divisor
  }
  var quotient = 0
  while dividend >= divisor {
    var temp = divisor
    var multiple = 1
    while dividend >= temp + temp {
      temp = temp + temp
      multiple = multiple + multiple
    }
    dividend = dividend - temp
    quotient = quotient + multiple
  }
  if negative {
    quotient = -quotient
  }
  if quotient > 2_147_483_647 {
    return 2_147_483_647
  }
  if quotient < (-2_147_483_647 - 1) {
    return -2_147_483_648
  }
  return quotient
}

func test_example_1() {
  expect(divide(10, 3) == 3)
}

func test_example_2() {
  expect(divide(7, -3) == (-2))
}

func test_overflow() {
  expect(divide(-2_147_483_648, -1) == 2_147_483_647)
}

func test_divide_by_1() {
  expect(divide(12345, 1) == 12345)
}

func test_negative_result() {
  expect(divide(-15, 2) == (-7))
}

func main() {
  test_example_1()
  test_example_2()
  test_overflow()
  test_divide_by_1()
  test_negative_result()
}
main()
