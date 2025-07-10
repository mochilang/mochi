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
val nums = mutableListOf(3, 1, 4)

fun main() {
    println(min(nums))
    println(max(nums))
}
