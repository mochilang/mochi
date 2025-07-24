import java.math.BigInteger

fun fib(n: Int): Int {
    if (n < 2) {
        return n
    }
    var a: Int = 0
    var b: Int = 1
    var i: Int = 1
    while (i < n) {
        val t: BigInteger = a + b
        a = b
        b = t as Int
        i = i + 1
    }
    return b
}

fun user_main(): Unit {
    for (n in mutableListOf(0, 1, 2, 3, 4, 5, 10, 40, 0 - 1)) {
        if (n < 0) {
            println("fib undefined for negative numbers")
        } else {
            println((("fib " + n.toString()) + " = ") + fib(n).toString())
        }
    }
}

fun main() {
    user_main()
}
