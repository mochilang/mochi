fun sum(list: List<Any?>): Int {
    var s = 0
    for (n in list) s += toInt(n)
    return s
}

fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val nums = mutableListOf(1, 2, 3)

val result = run {
    val __res = mutableListOf<Any?>()
    for (n in nums) {
        if (toBool(n > 1)) {
            __res.add(sum(n))
        }
    }
    __res
}

fun main() {
    println(result)
}
