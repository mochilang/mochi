fun fib(a: Int): Int {
    if (a < 2) {
        return a
    }
    return fib(a - 1) + fib(a - 2)
}

fun main() {
}
