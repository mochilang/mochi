fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun double_factorial_recursive(n: Int): Int {
    if (n < 0) {
        panic("double_factorial_recursive() not defined for negative values")
    }
    if (n <= 1) {
        return 1
    }
    return n * double_factorial_recursive(n - 2)
}

fun double_factorial_iterative(n: Int): Int {
    if (n < 0) {
        panic("double_factorial_iterative() not defined for negative values")
    }
    var result: Int = (1).toInt()
    var i: Int = (n).toInt()
    while (i > 0) {
        result = result * i
        i = i - 2
    }
    return result
}

fun test_double_factorial(): Unit {
    if (double_factorial_recursive(0) != 1) {
        panic("0!! recursive failed")
    }
    if (double_factorial_iterative(0) != 1) {
        panic("0!! iterative failed")
    }
    if (double_factorial_recursive(1) != 1) {
        panic("1!! recursive failed")
    }
    if (double_factorial_iterative(1) != 1) {
        panic("1!! iterative failed")
    }
    if (double_factorial_recursive(5) != 15) {
        panic("5!! recursive failed")
    }
    if (double_factorial_iterative(5) != 15) {
        panic("5!! iterative failed")
    }
    if (double_factorial_recursive(6) != 48) {
        panic("6!! recursive failed")
    }
    if (double_factorial_iterative(6) != 48) {
        panic("6!! iterative failed")
    }
    var n: Int = (0).toInt()
    while (n <= 10) {
        if (double_factorial_recursive(n) != double_factorial_iterative(n)) {
            panic("double factorial mismatch")
        }
        n = n + 1
    }
}

fun user_main(): Unit {
    test_double_factorial()
    println(double_factorial_iterative(10))
}

fun main() {
    user_main()
}
