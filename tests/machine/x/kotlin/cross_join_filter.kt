fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
data class Pair(var n: Any?, var l: Any?)

val nums = mutableListOf(1, 2, 3)

val letters = mutableListOf("A", "B")

val pairs = run {
    val __res = mutableListOf<Pair>()
    for (n in nums) {
        for (l in letters) {
            if (toBool(n % 2 == 0)) {
                __res.add(Pair(n = n, l = l))
            }
        }
    }
    __res
}

fun main() {
    println("--- Even pairs ---")
    for (p in pairs) {
        println(listOf(p.n, p.l).joinToString(" "))
    }
}
