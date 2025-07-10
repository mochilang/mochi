fun len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> 0
}
val prefix = "fore"

val s1 = "forest"

val s2 = "desert"

fun main() {
    println(s1.substring(0, len(prefix)) == prefix)
    println(s2.substring(0, len(prefix)) == prefix)
}
