fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val x = 5

fun main() {
    if (toBool(x > 3)) {
        println("big")
    }
    else {
        println("small")
    }
}
