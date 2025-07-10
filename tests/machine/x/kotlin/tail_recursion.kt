fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
fun sum_rec(n: Int, acc: Int): Int {
    if (toBool(n == 0)) {
        return acc
    }
    return sum_rec(n - 1, acc + n)
}

fun main() {
    println(sum_rec(10, 0))
}
