fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val nums = mutableListOf(1, 2, 3)

val letters = mutableListOf("A", "B")

val pairs = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (n in nums) {
        for (l in letters) {
            if (toBool(n % 2 == 0)) {
                __res.add((mutableMapOf("n" to n, "l" to l) as MutableMap<Any?, Any?>))
            }
        }
    }
    __res
}

fun main() {
    println("--- Even pairs ---")
    for (p in pairs) {
        println(listOf((p as MutableMap<*, *>)["n"], (p as MutableMap<*, *>)["l"]).joinToString(" "))
    }
}
