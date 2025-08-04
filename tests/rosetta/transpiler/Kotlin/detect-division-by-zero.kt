data class DivResult(var q: Int = 0, var ok: Boolean = false)
fun divCheck(x: Int, y: Int): DivResult {
    if (y == 0) {
        return DivResult(q = 0, ok = false)
    }
    return DivResult(q = x / y, ok = true)
}

fun printResult(r: DivResult): Unit {
    println((r.q.toString() + " ") + r.ok.toString())
}

fun user_main(): Unit {
    printResult(divCheck(3, 2))
    printResult(divCheck(3, 0))
}

fun main() {
    user_main()
}
