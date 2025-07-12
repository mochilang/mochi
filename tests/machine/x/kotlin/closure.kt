fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}
// Code generated from tests/vm/valid/closure.mochi

val add10 = makeAdder(10)

/**
 * Auto-generated from Mochi
 * @param n Int
 * @return (Int) -> Int
 */
fun makeAdder(n: Int): (Int) -> Int {
    return { x: Int -> toInt(x) + n }
}

fun main() {
    println(add10(7))
}
