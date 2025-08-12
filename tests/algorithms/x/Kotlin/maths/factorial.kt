fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun factorial(n: Int): Int {
    if (n < 0) {
        panic("factorial() not defined for negative values")
    }
    var value: Int = (1).toInt()
    var i: Int = (1).toInt()
    while (i <= n) {
        value = value * i
        i = i + 1
    }
    return value
}

fun factorial_recursive(n: Int): Int {
    if (n < 0) {
        panic("factorial() not defined for negative values")
    }
    if (n <= 1) {
        return 1
    }
    return n * factorial_recursive(n - 1)
}

fun test_factorial(): Unit {
    var i: Int = (0).toInt()
    while (i <= 10) {
        if (factorial(i) != factorial_recursive(i)) {
            panic("mismatch between factorial and factorial_recursive")
        }
        i = i + 1
    }
    if (factorial(6) != 720) {
        panic("factorial(6) should be 720")
    }
}

fun user_main(): Unit {
    test_factorial()
    println(factorial(6))
}

fun main() {
    user_main()
}
