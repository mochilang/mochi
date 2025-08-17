fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun signum(num: Double): Int {
    if (num < 0.0) {
        return 0 - 1
    }
    if (num > 0.0) {
        return 1
    }
    return 0
}

fun test_signum(): Unit {
    if (signum(5.0) != 1) {
        panic("signum(5) failed")
    }
    if (signum(0.0 - 5.0) != (0 - 1)) {
        panic("signum(-5) failed")
    }
    if (signum(0.0) != 0) {
        panic("signum(0) failed")
    }
    if (signum(10.5) != 1) {
        panic("signum(10.5) failed")
    }
    if (signum(0.0 - 10.5) != (0 - 1)) {
        panic("signum(-10.5) failed")
    }
    if (signum(0.000001) != 1) {
        panic("signum(1e-6) failed")
    }
    if (signum(0.0 - 0.000001) != (0 - 1)) {
        panic("signum(-1e-6) failed")
    }
    if (signum(123456789.0) != 1) {
        panic("signum(123456789) failed")
    }
    if (signum(0.0 - 123456789.0) != (0 - 1)) {
        panic("signum(-123456789) failed")
    }
}

fun user_main(): Unit {
    test_signum()
    println(signum(12.0))
    println(signum(0.0 - 12.0))
    println(signum(0.0))
}

fun main() {
    user_main()
}
