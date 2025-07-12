data class Counter(var n: Int)

var c = Counter(n = 0)

/**
 * Auto-generated from Mochi
 * @param c Counter
 */
fun inc(c: Counter): Unit {
    c.n = c.n + 1
}

fun main() {
    inc(c)
    println(c.n)
}
