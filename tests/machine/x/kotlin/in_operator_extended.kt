fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val xs = mutableListOf(1, 2, 3)

val ys = run {
    val __res = mutableListOf<Int>()
    for (x in xs) {
        if (toBool(x % 2 == 1)) {
            __res.add(x)
        }
    }
    __res
}

val m = mutableMapOf("a" to 1)

val s = "hello"

fun main() {
    println(1 in ys)
    println(2 in ys)
    println("a" in m)
    println("b" in m)
    println("ell" in s)
    println("foo" in s)
}
