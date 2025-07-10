val x = 8

val msg = if (toBool(x > 10)) "big" else if (toBool(x > 5)) "medium" else "small"

fun main() {
    println(msg)
}
