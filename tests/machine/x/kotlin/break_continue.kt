fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val numbers = mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9)

fun main() {
    for (n in numbers) {
        if (toBool(n % 2 == 0)) {
            continue
        }
        if (toBool(n > 7)) {
            break
        }
        println(listOf("odd number:", n).joinToString(" "))
    }
}
