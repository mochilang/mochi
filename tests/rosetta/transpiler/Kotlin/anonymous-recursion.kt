fun fib(n: Int): Int {
    if (n < 2) {
        return n
    }
    return fib(n - 1) + fib(n - 2)
}

fun user_main(): Unit {
    var i: Int = 0 - 1
    while (i <= 10) {
        if (i < 0) {
            println(("fib(" + i.toString()) + ") returned error: negative n is forbidden")
        } else {
            println((("fib(" + i.toString()) + ") = ") + fib(i).toString())
        }
        i = i + 1
    }
}

fun main() {
    user_main()
}
