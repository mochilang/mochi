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
fun main() {
    println(sum(mutableListOf(1, 2, 3)))
}
