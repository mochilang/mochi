fun ackermann(m: Int, n: Int): Int {
    if (m == 0) {
        return n + 1
    }
    if (n == 0) {
        return ackermann(m - 1, 1) as Int
    }
    return ackermann(m - 1, ackermann(m, n - 1) as Int) as Int
}

fun user_main(): Unit {
    println("A(0, 0) = " + ackermann(0, 0) as Int.toString())
    println("A(1, 2) = " + ackermann(1, 2) as Int.toString())
    println("A(2, 4) = " + ackermann(2, 4) as Int.toString())
    println("A(3, 4) = " + ackermann(3, 4) as Int.toString())
}

fun main() {
    user_main()
}
