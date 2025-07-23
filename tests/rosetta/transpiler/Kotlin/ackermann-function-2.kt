fun pow(base: Int, exp: Int): Int {
    var result: Int = 1
    var i: Int = 0
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun ackermann2(m: Int, n: Int): Int {
    if (m == 0) {
        return n + 1
    }
    if (m == 1) {
        return n + 2
    }
    if (m == 2) {
        return (2 * n) + 3
    }
    if (m == 3) {
        return (8 * pow(2, n) as Int) - 3
    }
    if (n == 0) {
        return ackermann2(m - 1, 1) as Int
    }
    return ackermann2(m - 1, ackermann2(m, n - 1) as Int) as Int
}

fun user_main(): Unit {
    println("A(0, 0) = " + ackermann2(0, 0) as Int.toString())
    println("A(1, 2) = " + ackermann2(1, 2) as Int.toString())
    println("A(2, 4) = " + ackermann2(2, 4) as Int.toString())
    println("A(3, 4) = " + ackermann2(3, 4) as Int.toString())
}

fun main() {
    user_main()
}
