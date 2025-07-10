fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
var i = 0

fun main() {
    while (toBool(i < 3)) {
        println(i)
        i = i + 1
    }
}
