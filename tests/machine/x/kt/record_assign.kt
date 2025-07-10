data class Counter(var n: Int)

fun inc(c: Counter) {
    c.n += 1
}

fun main() {
    val c = Counter(0)
    inc(c)
    println(c.n)
}
