// Code generated from tests/vm/valid/map_in_operator.mochi

val m = mutableMapOf(1 to "a", 2 to "b")

fun main() {
    println(1 in m)
    println(3 in m)
}
