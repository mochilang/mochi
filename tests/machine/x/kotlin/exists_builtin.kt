fun exists(list: Collection<Any?>): Boolean = list.isNotEmpty()

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
val data = mutableListOf(1, 2)

val flag = exists(run {
    val __res = mutableListOf<Int>()
    for (x in data) {
        if (toBool(x == 1)) {
            __res.add(x)
        }
    }
    __res
})

fun main() {
    println(flag)
}
