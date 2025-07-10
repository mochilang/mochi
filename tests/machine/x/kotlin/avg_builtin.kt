fun avg(list: List<Any?>): Double {
    if (list.isEmpty()) return 0.0
    var s = 0.0
    for (n in list) s += toDouble(n)
    return s / list.size
}

fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}
fun main() {
    println(avg(mutableListOf(1, 2, 3)))
}
