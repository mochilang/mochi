// Code generated from tests/vm/valid/map_membership.mochi

val m = mutableMapOf("a" to 1, "b" to 2)

fun main() {
    println("a" in m)
    println("c" in m)
}
