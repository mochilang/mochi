// Code generated from tests/vm/valid/if_then_else_nested.mochi

val x = 8

val msg = if (x > 10) "big" else if (x > 5) "medium" else "small"

fun main() {
    println(msg)
}
