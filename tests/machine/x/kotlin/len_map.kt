fun len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> 0
}
fun main() {
    println(len(mutableMapOf("a" to 1, "b" to 2)))
}
