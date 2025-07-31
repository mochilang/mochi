data class Counter(var n: Int)
fun inc(c: Counter): Unit {
    c.n = c.n + 1
}

fun main() {
    var c: Counter = Counter(n = 0)
    inc(c)
    println(c.n)
}
