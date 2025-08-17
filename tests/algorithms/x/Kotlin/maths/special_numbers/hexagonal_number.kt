fun panic(msg: String): Nothing { throw RuntimeException(msg) }

var samples: MutableList<Int> = mutableListOf(4, 11, 22)
fun hexagonal(n: Int): Int {
    if (n < 1) {
        panic("Input must be a positive integer")
    }
    return n * ((2 * n) - 1)
}

fun main() {
    for (s in samples) {
        println(hexagonal(s))
    }
}
