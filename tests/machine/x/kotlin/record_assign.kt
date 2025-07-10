data class Counter(var n: Int)

var c = Counter(n = 0)

fun inc(c: Counter): Unit {
    c.n = c.n + 1
}

fun main() {
    inc(c)
    println(c.n)
}
