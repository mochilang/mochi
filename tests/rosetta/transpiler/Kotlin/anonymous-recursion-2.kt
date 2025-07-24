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
    for (i in mutableListOf(0 - 1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) {
        if (i < 0) {
            println(("fib(" + i.toString()) + ") returned error: negative n is forbidden")
        } else {
            println((("fib(" + i.toString()) + ") = ") + fib(i).toString())
        }
    }
}

fun main() {
    user_main()
}
