fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun triangular_number(position: Int): Int {
    if (position < 0) {
        panic("position must be non-negative")
    }
    return (position * (position + 1)) / 2
}

fun test_triangular_number(): Unit {
    if (triangular_number(1) != 1) {
        panic("triangular_number(1) failed")
    }
    if (triangular_number(3) != 6) {
        panic("triangular_number(3) failed")
    }
}

fun user_main(): Unit {
    test_triangular_number()
    println(triangular_number(10))
}

fun main() {
    user_main()
}
