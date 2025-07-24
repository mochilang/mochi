fun ackermann(m: Int, n: Int): Int {
    if (m == 0) {
        return n + 1
    }
    if (n == 0) {
        return ackermann(m - 1, 1)
    }
    return ackermann(m - 1, ackermann(m, n - 1))
}

fun user_main(): Unit {
    println("A(0, 0) = " + ackermann(0, 0).toString())
    println("A(1, 2) = " + ackermann(1, 2).toString())
    println("A(2, 4) = " + ackermann(2, 4).toString())
    println("A(3, 4) = " + ackermann(3, 4).toString())
}

fun main() {
    user_main()
}
