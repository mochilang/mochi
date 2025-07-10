fun max(list: List<Any?>): Int {
    var m = Int.MIN_VALUE
    for (n in list) {
        val v = toInt(n)
        if (v > m) m = v
    }
    return if (m == Int.MIN_VALUE) 0 else m
}

fun min(list: List<Any?>): Int {
    var m = Int.MAX_VALUE
    for (n in list) {
        val v = toInt(n)
        if (v < m) m = v
    }
    return if (m == Int.MAX_VALUE) 0 else m
}

fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}
val nums = mutableListOf(3, 1, 4)

fun main() {
    println(min(nums))
    println(max(nums))
}
