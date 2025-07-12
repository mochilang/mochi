// Code generated from tests/vm/valid/values_builtin.mochi

val m = mutableMapOf("a" to 1, "b" to 2, "c" to 3)

fun main() {
    println(m.values.toMutableList())
}
