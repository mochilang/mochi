fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun power(base: Int, exponent: Int): Int {
    if (exponent == 0) {
        return 1
    }
    return base * power(base, exponent - 1)
}

fun test_power(): Unit {
    if (power(3, 4) != 81) {
        panic("power(3,4) failed")
    }
    if (power(2, 0) != 1) {
        panic("power(2,0) failed")
    }
    if (power(5, 6) != 15625) {
        panic("power(5,6) failed")
    }
}

fun user_main(): Unit {
    test_power()
    println(power(3, 4))
}

fun main() {
    user_main()
}
