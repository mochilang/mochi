fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}
val add10 = makeAdder(10)

fun makeAdder(n: Int): (Int) -> Int {
    return { x: Int -> toInt(x) + n }
}

fun main() {
    println(add10(7))
}
