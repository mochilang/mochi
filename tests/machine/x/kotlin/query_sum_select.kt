fun sum(list: List<Any?>): Int {
    var s = 0
    for (n in list) s += toInt(n)
    return s
}
val nums = mutableListOf(1, 2, 3)

val result = run {
    val __res = mutableListOf<Any?>()
    for (n in nums) {
        if (n > 1) {
            __res.add(sum(n))
        }
    }
    __res
}

fun main() {
    println(result)
}
