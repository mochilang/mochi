fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}
// Code generated from tests/vm/valid/fun_expr_in_let.mochi

val square = { x: Int -> toDouble(x) * toDouble(x) }

fun main() {
    println(square(6))
}
